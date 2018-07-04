[<RequireQualifiedAccess>]
module Dap.Archive.WebSocket.Accessor.Logic

open System.Threading
open Dap.Prelude
open Dap.Platform
open Dap.WebSocket
open Dap.Archive.WebSocket.Accessor.Types
open Dap.Archive.WebSocket.Accessor.Tasks
module WebSocketTypes = Dap.WebSocket.Types
module WebSocketClientTypes = Dap.WebSocket.Client.Types

let private doSetup req ((uri, callback) : string * Callback<unit>) : PartOperate<'pkt> =
    fun runner (model, cmd) ->
        match model.Uri with
        | Some uri' ->
            reply runner callback <| nak req "Already_Setup" (uri', uri)
            (model, cmd)
        | None ->
            replyAsync4 runner req callback nakOnFailed <| doSetupAsync
            (runner, model, cmd)
            |=|> updateModel (fun m -> {m with Uri = Some uri})

let private doStart req (callback : Callback<WebSocketTypes.ConnectedStats option>) : PartOperate<'pkt> =
    fun runner (model, cmd) ->
        match model.Uri with
        | Some uri ->
            match model.Running with
            | true ->
                reply runner callback <| nak req "Already_Running" model
                (model, cmd)
            | false ->
                match model.Client with
                | None ->
                    reply runner callback <| nak req "Setup_Not_Succeed" model
                    (model, cmd)
                | Some client ->
                    if client.Actor.State.Connected then
                        reply runner callback <| ack req None
                    else
                        replyAsync4 runner req callback doStartFailed doStartAsync
                    (runner, model, cmd)
                    |=|> updateModel (fun m -> {m with Running = true})
        | None ->
            reply runner callback <| nak req "Not_Setup" ()
            (model, cmd)

let private doStop req (callback : Callback<unit>) : PartOperate<'pkt> =
    fun runner (model, cmd) ->
        match model.Uri with
        | Some _uri ->
            match model.Running with
            | false ->
                reply runner callback <| nak req "Not_Running" model
                (model, cmd)
            | true ->
                match model.Client with
                | None ->
                    reply runner callback <| nak req "Setup_Not_Succeed" model
                    (model, cmd)
                | Some client ->
                    if not client.Actor.State.Connected || client.Actor.State.Closing then
                        reply runner callback <| ack req ()
                    else
                        replyAsync4 runner req callback nakOnFailed doStopAsync
                    (runner, model, cmd)
                    |=|> updateModel (fun m -> {m with Running = false})
        | None ->
            reply runner callback <| nak req "Not_Setup" ()
            (model, cmd)

let private doSend req ((pkt, callback) : 'pkt * Callback<WebSocketTypes.SendStats>) : PartOperate<'pkt> =
    fun runner (model, cmd) ->
        match model.Client with
        | None ->
            reply runner callback <| nak req "Client_Not_Exist" model.Uri
        | Some client ->
            if not client.Actor.State.Connected then
                reply runner callback <| nak req "Client_Not_Connected" model.Uri
            elif client.Actor.State.Closing then
                reply runner callback <| nak req "Client_Closing" model.Uri
            else
                replyAsync4 runner req callback nakOnFailed <| doSendAsync pkt
        (model, cmd)

let private handleReq req : PartOperate<'pkt> =
    fun runner (model, cmd) ->
        match req with
        | DoSetup (a, b) -> doSetup req (a, b)
        | DoStart a -> doStart req a
        | DoStop a -> doStop req a
        | DoSend (a, b) -> doSend req (a, b)
        <| runner <| (model, cmd)

let private onClientEvent (runner : Part<'pkt>)
                            : WebSocketTypes.Evt<'pkt> -> unit =
    fun evt ->
        match evt with
        | WebSocketTypes.OnSent (stats, pkt) ->
            runner.Deliver <| AccessorEvt ^<| OnSent (stats, pkt)
        | WebSocketTypes.OnReceived (stats, pkt) ->
            runner.Deliver <| AccessorEvt ^<| OnReceived (stats, pkt)
        | WebSocketTypes.OnConnected stats ->
            runner.Deliver <| InternalEvt ^<| OnConnected stats
        | WebSocketTypes.OnDisconnected stats ->
            runner.Deliver <| InternalEvt ^<| OnDisconnected stats

let private tryReconnect : PartOperate<'pkt> =
    fun runner (model, cmd) ->
        match runner.Actor.Args.RetryDelay with
        | None ->
            (model, cmd)
        | Some delay ->
            (runner, model, cmd)
            |=|> addFutureCmd delay ^<| InternalEvt DoReconnect

let private doReconnect : PartOperate<'pkt> =
    fun runner (model, cmd) ->
        if runner.Actor.State.Running then
            let client = runner.Actor.State.Client |> Option.get
            let uri = runner.Actor.State.Uri |> Option.get
            let cts = runner.Actor.State.Cts
            client.Post <| WebSocketClientTypes.DoConnect' uri cts.Token None
        (model, cmd)

let private handleInternalEvt evt : PartOperate<'pkt> =
    fun runner (model, cmd) ->
        match evt with
        | OnSetup (req, callback, client, recorder) ->
            client.Actor.OnEvent.AddWatcher runner "onClientEvent" <| onClientEvent runner
            replyAfter runner callback <| ack req ()
            (runner, model, cmd)
            |=|> updateModel (fun m -> {m with Client = Some client ; Recorder = recorder})
        | OnConnected stats ->
            match runner.Actor.Args.OnConnectedAsync with
            | None -> ()
            | Some handler ->
                runner.AddTask4 ignoreOnFailed <| callOnConnectedAsync handler
            (runner, model, cmd)
            |=|> addCmd ^<| AccessorEvt ^<| OnStarted stats
        | OnDisconnected stats ->
            (runner, model, cmd)
            |-|> updateModel (fun m -> {m with Cts = new CancellationTokenSource ()})
            |-|> addCmd ^<| AccessorEvt ^<| OnStopped stats
            |=|> tryReconnect
        | DoReconnect ->
            (runner, model, cmd)
            |=|> doReconnect

let private update : PartUpdate<Args<'pkt>, Model<'pkt>, Msg<'pkt>, Req<'pkt>, Evt<'pkt>> =
    fun runner model msg ->
        match msg with
        | AccessorReq req -> handleReq req
        | AccessorEvt _evt -> noOperation
        | InternalEvt evt -> handleInternalEvt evt
        <| runner <| (model, [])

let private init : PartInit<Args<'pkt>, Model<'pkt>, Msg<'pkt>> =
    fun _runner _args ->
        ({
            Uri = None
            Cts = new CancellationTokenSource ()
            Client = None
            Recorder = None
            Running = false
        }, noCmd)

let getSpec<'pkt> (newArgs : PartNewArgs<Args<'pkt>>) : PartSpec<Args<'pkt>, Model<'pkt>, Msg<'pkt>, Req<'pkt>, Evt<'pkt>> =
    {
        Init = init
        Update = update
        NewArgs = newArgs
        WrapReq = AccessorReq
        CastEvt = castEvt<'pkt>
    }
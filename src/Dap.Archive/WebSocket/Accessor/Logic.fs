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

let private doSetup req ((uri, callback) : string * Callback<unit>) : PartOperate<'actorMsg, 'pkt> =
    fun runner (model, cmd) ->
        match model.Uri with
        | Some uri' ->
            reply runner callback <| nak req "Already_Setup" (uri', uri)
            (model, cmd)
        | None ->
            replyAsync runner req callback nakOnFailed <| doSetupAsync
            (runner, model, cmd)
            |=|> updateModel (fun m -> {m with Uri = Some uri})

let private doSetUri req ((uri, callback) : string * Callback<unit>) : PartOperate<'actorMsg, 'pkt> =
    fun runner (model, cmd) ->
        match model.Uri with
        | Some uri' ->
            if uri' = uri then
                reply runner callback <| nak req "Same_Uri" uri
                (model, cmd)
            else
                //TODO: check current connection, maybe reconnect
                (runner, model, cmd)
                |=|> updateModel (fun m -> {m with Uri = Some uri})
        | None ->
            reply runner callback <| nak req "Not_Setup" ()
            (model, cmd)

let private doStart req (callback : Callback<WebSocketTypes.LinkedStats option>) : PartOperate<'actorMsg, 'pkt> =
    fun runner (model, cmd) ->
        match model.Uri with
        | Some uri ->
            match model.Running with
            | true ->
                reply runner callback <| ack req None
                (model, cmd)
            | false ->
                match model.Client with
                | None ->
                    reply runner callback <| nak req "Setup_Not_Succeed" model
                    (model, cmd)
                | Some client ->
                    match client.Actor.State.Status with
                    | LinkStatus.Linking
                    | LinkStatus.Linked ->
                        reply runner callback <| ack req None
                    | _ ->
                        replyAsync runner req callback doStartFailed doStartAsync
                    (runner, model, cmd)
                    |=|> updateModel (fun m -> {m with Running = true})
        | None ->
            reply runner callback <| nak req "Not_Setup" ()
            (model, cmd)

let private doStop req (callback : Callback<unit>) : PartOperate<'actorMsg, 'pkt> =
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
                    match client.Actor.State.Status with
                    | LinkStatus.Closing
                    | LinkStatus.Closed ->
                        reply runner callback <| ack req ()
                    | _ ->
                        replyAsync runner req callback nakOnFailed doStopAsync
                    (runner, model, cmd)
                    |=|> updateModel (fun m -> {m with Running = false})
        | None ->
            reply runner callback <| nak req "Not_Setup" ()
            (model, cmd)

let private doSend req ((pkt, callback) : 'pkt * Callback<WebSocketTypes.SendStats>) : PartOperate<'actorMsg, 'pkt> =
    fun runner (model, cmd) ->
        match model.Client with
        | None ->
            reply runner callback <| nak req "Client_Not_Exist" model.Uri
        | Some client ->
            match client.Actor.State.Status with
            | LinkStatus.Linked ->
                replyAsync runner req callback nakOnFailed <| doSendAsync pkt
            | _ ->
                reply runner callback <| nak req "Client_Not_Linked" model.Uri
        (model, cmd)

let private handleReq req : PartOperate<'actorMsg, 'pkt> =
    fun runner (model, cmd) ->
        match req with
        | DoSetup (a, b) -> doSetup req (a, b)
        | DoSetUri (a, b) -> doSetUri req (a, b)
        | DoStart a -> doStart req a
        | DoStop a -> doStop req a
        | DoSend (a, b) -> doSend req (a, b)
        <| runner <| (model, cmd)

let private onStatusChanged (runner : Part<'actorMsg, 'pkt>) (status : LinkStatus) : unit =
    match status with
    | LinkStatus.Linked ->
        let client = runner.Part.State.Client |> Option.get
        let stats =
            client.Actor.State.Stats
            |> Option.map (fun s -> s.Linked)
        runner.Deliver <| InternalEvt ^<| OnLinked stats
    | LinkStatus.Closed ->
        let client = runner.Part.State.Client |> Option.get
        let stats = client.Actor.State.Stats
        runner.Deliver <| InternalEvt ^<| OnClosed stats
    | _ ->
        ()

let private onClientEvent (runner : Part<'actorMsg, 'pkt>)
                            : WebSocketTypes.Evt<'pkt> -> unit =
    fun evt ->
        match evt with
        | WebSocketTypes.OnSent (stats, pkt) ->
            runner.Deliver <| AccessorEvt ^<| OnSent (stats, pkt)
        | WebSocketTypes.OnReceived (stats, pkt) ->
            runner.Deliver <| AccessorEvt ^<| OnReceived (stats, pkt)
        | WebSocketTypes.OnStatusChanged status ->
            onStatusChanged runner status

let private tryReconnect : PartOperate<'actorMsg, 'pkt> =
    fun runner (model, cmd) ->
        match runner.Part.Args.RetryDelay with
        | None ->
            (model, cmd)
        | Some delay ->
            match model.Reconnecting with
            | true -> (model, cmd)
            | false ->
                (runner, model, cmd)
                |-|> updateModel (fun m -> {m with Reconnecting = true})
                |=|> addFutureCmd delay ^<| InternalEvt DoReconnect

let private doReconnect : PartOperate<'actorMsg, 'pkt> =
    fun runner (model, cmd) ->
        (runner, model, cmd)
        |-|> (
            if not runner.Part.State.Running then
                runner.AddTask closeOnFailed doReconnectAsync
                updateModel (fun m -> {m with Running = true})
            else
                noOperation
        )|=|> updateModel (fun m -> {m with Reconnecting = false})

let private handleInternalEvt evt : PartOperate<'actorMsg, 'pkt> =
    fun runner (model, cmd) ->
        match evt with
        | OnSetup (req, callback, client, recorder) ->
            client.Actor.OnEvent.AddWatcher runner "onClientEvent" <| onClientEvent runner
            replyAfter runner callback <| ack req ()
            (runner, model, cmd)
            |=|> updateModel (fun m -> {m with Client = Some client ; Recorder = recorder})
        | OnLinked stats ->
            match runner.Part.Args.OnLinkedAsync with
            | None -> ()
            | Some handler ->
                runner.AddTask ignoreOnFailed <| callOnLinkedAsync handler
            (runner, model, cmd)
            |=|> addCmd ^<| AccessorEvt ^<| OnStarted stats
        | OnClosed stats ->
            (runner, model, cmd)
            |-|> updateModel (fun m ->
                {m with
                    Cts = new CancellationTokenSource ()
                    Running = false
                }
            )|-|> addCmd ^<| AccessorEvt ^<| OnStopped stats
            |=|> tryReconnect
        | DoReconnect ->
            (runner, model, cmd)
            |=|> doReconnect

let private update : ActorUpdate<Part<'actorMsg, 'pkt>, Args<'pkt>, Model<'pkt>, Msg<'pkt>, Req<'pkt>, Evt<'pkt>> =
    fun runner msg model ->
        match msg with
        | AccessorReq req -> handleReq req
        | AccessorEvt _evt -> noOperation
        | InternalEvt evt -> handleInternalEvt evt
        <| runner <| (model, [])

let private init : ActorInit<Args<'pkt>, Model<'pkt>, Msg<'pkt>> =
    fun _runner _args ->
        ({
            Uri = None
            Cts = new CancellationTokenSource ()
            Client = None
            Recorder = None
            Running = false
            Reconnecting = false
        }, noCmd)

let spec<'actorMsg, 'pkt when 'actorMsg :> IMsg> (args : Args<'pkt>) =
    new ActorSpec<Part<'actorMsg, 'pkt>, Args<'pkt>, Model<'pkt>, Msg<'pkt>, Req<'pkt>, Evt<'pkt>>
        (Part<'actorMsg, 'pkt>.Spawn, args, AccessorReq, castEvt<'pkt>, init, update)
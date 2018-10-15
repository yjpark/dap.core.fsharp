[<RequireQualifiedAccess>]
module Dap.WebSocket.Internal.Logic

open System.Net.WebSockets

open Dap.Prelude
open Dap.Platform
open Dap.WebSocket
open Dap.WebSocket.Types
open Dap.WebSocket.Internal.Tasks

let internal doSend (runner : Agent<'socket, 'pkt, 'req>)
                    req ((pkt, callback) : 'pkt * Callback<unit>) : unit =
    match runner.Actor.State.Link with
    | Some state ->
        match state.Socket.State with
        | WebSocketState.Open ->
            replyAsync runner req callback nakOnFailed <| doSendAsync pkt
        | state ->
            reply runner callback <| nak req "Invalid_State" state
    | None ->
        reply runner callback <| nak req "Not_Connected" None

let linkStatusOfSocket (socket : WebSocket) =
    match socket.State with
    | WebSocketState.None ->
        LinkStatus.NoLink
    | WebSocketState.Connecting ->
        LinkStatus.Linking
    | WebSocketState.Open ->
        LinkStatus.Linked
    | WebSocketState.CloseSent
    | WebSocketState.CloseReceived ->
        LinkStatus.Closing
    | WebSocketState.Closed
    | WebSocketState.Aborted ->
        LinkStatus.Closed
    | _ ->
        LinkStatus.Unknown

let private doSetStatus (status : LinkStatus)  : ActorOperate<'socket, 'pkt, 'req> =
    fun runner (model, cmd) ->
        (runner, model, cmd)
        |-|> updateModel (fun m -> {m with Status = status})
        |=|> addSubCmd WebSocketEvt ^<| OnStatusChanged status

let private doRefreshStatus (err : exn option) : ActorOperate<'socket, 'pkt, 'req> =
    fun runner (model, cmd) ->
        let status =
            model.Link
            |> Option.map (fun l -> linkStatusOfSocket l.Socket)
            |> Option.defaultValue LinkStatus.NoLink
        match err with
        | None ->
            if (status <> model.Status) then
                logInfo runner "Link" "Refresh_Status" (model.Status, status, model.Link)
        | Some e ->
            logException runner "Link" "Refresh_Status" (model.Status, status, model.Link) e
        (runner, model, cmd)
        |-|> updateModel (fun m -> {m with NextRefreshTime =  runner.Clock.Now + runner.Actor.Args.RefreshInterval})
        |=|>
            if status <> model.Status then
                doSetStatus status
            else
                noOperation

let private tryCloseSocket : ActorOperate<'socket, 'pkt, 'req> =
    fun runner (model, cmd) ->
        runner.AddTask refreshStatusOnFailed tryCloseSocketAsync
        (model, cmd)

let private onLinked : ActorOperate<'socket, 'pkt, 'req> =
    fun runner (model, cmd) ->
        (runner, model, cmd)
        |=|> addSubCmd InternalEvt ^<| DoRefreshStatus None

let private onTick ((time, delta) : Instant * Duration) : ActorOperate<'socket, 'pkt, 'req> =
    fun runner (model, cmd) ->
        if runner.Clock.Now > model.NextRefreshTime then
            (runner, model, cmd)
            |-|> updateModel (fun m -> {m with NextRefreshTime =  runner.Clock.Now + runner.Actor.Args.RefreshInterval})
            |=|> addSubCmd InternalEvt ^<| DoRefreshStatus None
        else
            (model, cmd)

let private handleEvt evt : ActorOperate<'socket, 'pkt, 'req> =
    fun runner (model, cmd) ->
        match evt with
        | OnStatusChanged status ->
            runner.LinkStats.AddStatus runner status
        | _ -> ()
        (model, cmd)

let private update : Update<Agent<'socket, 'pkt, 'req>, Model<'socket, 'pkt>, Msg<'pkt, 'req>> =
    fun runner msg model ->
        match msg with
        | WebSocketReq req -> runner.Actor.Args.HandleReq req
        | WebSocketEvt evt -> handleEvt evt
        | InternalEvt evt ->
            match evt with
            | DoRefreshStatus err -> doRefreshStatus err
            | TryCloseSocket -> tryCloseSocket
            | OnLinked -> onLinked
            | OnTick (a, b) -> onTick (a, b)
        <| runner <| (model, [])

let private init : ActorInit<Args<'socket, 'pkt, 'req>, Model<'socket, 'pkt>, Msg<'pkt, 'req>> =
    fun runner args ->
        ({
            Link = None
            Status = LinkStatus.NoLink
            NextRefreshTime = runner.Clock.Now + args.RefreshInterval
        }, noCmd)

let private subscribe : Subscribe<Agent<'socket, 'pkt, 'req>, Model<'socket, 'pkt>, Msg<'pkt, 'req>> =
    fun runner _model ->
        runner.Pack.Ticker.WatchOnTick runner "OnTick" (runner.Deliver << InternalEvt << OnTick)
        noCmd

let spec pack (args : Args<'socket, 'pkt, 'req>) =
    new ActorSpec<Agent<'socket, 'pkt, 'req>, Args<'socket, 'pkt, 'req>, Model<'socket, 'pkt>, Msg<'pkt, 'req>, 'req, Evt<'pkt>>
        (Agent<'socket, 'pkt, 'req>.Spawn pack, args, WebSocketReq, castEvt<'pkt, 'req>, init, update)
    |> withSubscribe subscribe

[<RequireQualifiedAccess>]
module Dap.WebSocket.Internal.Logic

open System.Net.WebSockets

open Dap.Prelude
open Dap.Platform
open Dap.WebSocket.Const
open Dap.WebSocket.Types
open Dap.WebSocket.Internal.Tasks

let internal doSend (runner : Agent<'socket, 'pkt, 'req>)
                    req ((pkt, callback) : 'pkt * Callback<SendStats>) : unit =
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
            logInfo runner "Link" "Refresh_Status" (model.Status, status, model.Link, model.Stats)
        | Some e ->
            logException runner "Link" "Refresh_Status" (model.Status, status, model.Link, model.Stats) e
        if status <> model.Status then
            (runner, model, cmd)
            |=|> doSetStatus status
        else
            (model, cmd)

let private tryCloseSocket : ActorOperate<'socket, 'pkt, 'req> =
    fun runner (model, cmd) ->
        runner.AddTask refreshStatusOnFailed tryCloseSocketAsync
        (model, cmd)

let private onLinked (stats : LinkedStats) : ActorOperate<'socket, 'pkt, 'req> =
    fun runner (model, cmd) ->
        let stats = ConnectionStats.Create stats
        (runner, model, cmd)
        |-|> updateModel (fun m -> {m with Stats = Some stats})
        |=|> addSubCmd InternalEvt ^<| DoRefreshStatus None

let private handleEvt evt : ActorOperate<'socket, 'pkt, 'req> =
    fun runner (model, cmd) ->
        match evt with
        | OnReceived (_stats, _pkt) ->
            model.Stats
            |> Option.iter (fun stats ->
                stats.ReceivedCount <- stats.ReceivedCount + 1
            )
            noOperation
        | OnSent (_stats, _pkt) ->
            model.Stats
            |> Option.iter (fun stats ->
                stats.SentCount <- stats.SentCount + 1
            )
            noOperation
        | OnStatusChanged status ->
            match status with
            | LinkStatus.Closed ->
                let stats =
                    model.Stats
                    |> Option.map (fun stats ->
                        {stats with ClosedTime = Some runner.Clock.Now}
                    )
                updateModel (fun m -> {m with Stats = stats})
            | _ ->
                noOperation
        <| runner <| (model, cmd)

let private update : ActorUpdate<Agent<'socket, 'pkt, 'req>, Args<'socket, 'pkt, 'req>, Model<'socket, 'pkt>, Msg<'pkt, 'req>, 'req, Evt<'pkt>> =
    fun runner msg model ->
        match msg with
        | WebSocketReq req -> runner.Actor.Args.HandleReq req
        | WebSocketEvt evt -> handleEvt evt
        | InternalEvt evt ->
            match evt with
            | DoRefreshStatus err -> doRefreshStatus err
            | TryCloseSocket -> tryCloseSocket
            | OnLinked stats -> onLinked stats
        <| runner <| (model, [])

let private init : ActorInit<Args<'socket, 'pkt, 'req>, Model<'socket, 'pkt>, Msg<'pkt, 'req>> =
    fun _runner _args ->
        ({
            Link = None
            Stats = None
            Status = LinkStatus.NoLink
        }, noCmd)

let spec (args : Args<'socket, 'pkt, 'req>) =
    new ActorSpec<Agent<'socket, 'pkt, 'req>, Args<'socket, 'pkt, 'req>, Model<'socket, 'pkt>, Msg<'pkt, 'req>, 'req, Evt<'pkt>>
        (Agent<'socket, 'pkt, 'req>.Spawn, args, WebSocketReq, castEvt<'pkt, 'req>, init, update)



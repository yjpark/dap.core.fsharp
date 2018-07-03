[<RequireQualifiedAccess>]
module Dap.WebSocket.Internal.Logic

open System.Net.WebSockets

open Elmish
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
            replyAsync3 runner req callback nakOnFailed <| doSendAsync pkt
        | state ->
            reply runner callback <| nak req "Invalid_State" state
    | None ->
        reply runner callback <| nak req "Not_Connected" None

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
        | OnConnected stats ->
            updateModel (fun m -> {m with Stats = Some (ConnectionStats.Create stats) ; Closing = false})
        | OnDisconnected _stats ->
            updateModel (fun m -> {m with Link = None ; Stats = None ; Closing = false})
        <| runner <| (model, cmd)

let private update : ActorUpdate<Args<'socket, 'pkt, 'req>, Model<'socket, 'pkt>, Msg<'pkt, 'req>, 'req, Evt<'pkt>> =
    fun runner model msg ->
        match msg with
        | WebSocketReq req -> runner.Actor.Args.HandleReq req
        | WebSocketEvt evt -> handleEvt evt
        <| runner <| (model, [])

let private init : ActorInit<Args<'socket, 'pkt, 'req>, Model<'socket, 'pkt>, Msg<'pkt, 'req>, 'req, Evt<'pkt>> =
    fun _runner _args ->
        ({
            Link = None
            Stats = None
            Closing = false
        }, noCmd)

let logic : ActorLogic<Args<'socket, 'pkt, 'req>, Model<'socket, 'pkt>, Msg<'pkt, 'req>, 'req, Evt<'pkt>> =
    {
        Init = init
        Update = update
        Subscribe = noSubscription
    }

let getSpec (newArgs : ActorNewArgs<Args<'socket, 'pkt, 'req>>) : AgentSpec<Args<'socket, 'pkt, 'req>, Model<'socket, 'pkt>, Msg<'pkt, 'req>, 'req, Evt<'pkt>> =
    {
        Actor =
            {
                NewArgs = newArgs
                Logic = logic
                WrapReq = WebSocketReq
                CastEvt = castEvt<'pkt, 'req>
            }
        OnAgentEvent = None
        GetSlowCap = Some <| getRemoteSlowCap DefaultWebSocketReplySlowCap
    }

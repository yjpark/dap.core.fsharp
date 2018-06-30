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
                    (msg : IMsg) ((pkt, callback) : 'pkt * Callback<SendStats>) : unit =
    match runner.Actor.State.Link with
    | Some state ->
        match state.Socket.State with
        | WebSocketState.Open ->
            replyAsync3 runner msg callback nakOnFailed <| doSendAsync pkt
        | state ->
            reply runner callback <| nak msg "Invalid_State" state
    | None ->
        reply runner callback <| nak msg "Not_Connected" None

let private handleEvt _msg evt : ActorOperate<'socket, 'pkt, 'req> =
    fun runner (model, cmd) ->
        match evt with
        | OnConnected ->
            runner.RunTask3 doReceiveFailed doReceiveAsync
            setModel {model with Connected = true}
        | OnDisconnected ->
            setModel {model with Link = None ; Connected = false}
        | _ -> noOperation
        <| runner <| (model, cmd)

let private update : ActorUpdate<Args<'socket, 'pkt, 'req>, Model<'socket, 'pkt>, Msg<'pkt, 'req>, 'req, Evt<'pkt>> =
    fun runner model msg ->
        match msg with
        | WebSocketReq req -> runner.Actor.Args.HandleReq (msg :> IMsg) req
        | WebSocketEvt evt -> handleEvt msg evt
        <| runner <| (model, [])

let private init : ActorInit<Args<'socket, 'pkt, 'req>, Model<'socket, 'pkt>, Msg<'pkt, 'req>, 'req, Evt<'pkt>> =
    fun _runner args ->
        ({
            Link = None
            Connected = false
        }, Cmd.none)

let private subscribe : ActorSubscribe<Args<'socket, 'pkt, 'req>, Model<'socket, 'pkt>, Msg<'pkt, 'req>, 'req, Evt<'pkt>> =
    fun runner model ->
        subscribeEvent runner model WebSocketEvt runner.Actor.Args.OnEvent

let logic : ActorLogic<Args<'socket, 'pkt, 'req>, Model<'socket, 'pkt>, Msg<'pkt, 'req>, 'req, Evt<'pkt>> =
    {
        Init = init
        Update = update
        Subscribe = subscribe
    }

let getSpec (newArgs : NewArgs<Args<'socket, 'pkt, 'req>>) : AgentSpec<Args<'socket, 'pkt, 'req>, Model<'socket, 'pkt>, Msg<'pkt, 'req>, 'req, Evt<'pkt>> =
    {
        Actor =
            {
                NewArgs = newArgs
                Logic = logic
                WrapReq = WebSocketReq
                GetOnEvent = fun args -> args.OnEvent
            }
        OnAgentEvent = None
        GetSlowCap = Some <| getRemoteSlowCap DefaultWebSocketReplySlowCap
    }

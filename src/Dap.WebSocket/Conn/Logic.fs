[<RequireQualifiedAccess>]
module Dap.WebSocket.Conn.Logic

open System
open System.Threading.Tasks
open System.Net.WebSockets
open FSharp.Control.Tasks
open Elmish
open Dap.Prelude
open Dap.Platform
open Dap.WebSocket
open Dap.WebSocket.Conn.Types
open Dap.WebSocket.Conn.Tasks
module BaseLogic = Dap.WebSocket.Internal.Logic

let private doConnect msg (ident, token, socket, callback) : Operate<IAgent, Model<'pkt>, Msg<'pkt>> =
    fun runner (model, cmd) ->
        match model.State with
        | Some state ->
            reply runner callback <| nak msg "Can_Not_Connect" state.Socket.State
            noOperation
        | None ->
            let state : State<'pkt> = {
                Args = model.Args
                FireEvent = model.Args.FireEvent'
                Ident = ident
                Token = token
                Socket = socket
                Buffer = Array.create<byte> model.Args.BufferSize 0uy
            }
            let task = doReceiveAsync state runner
            reply runner callback <| ack msg (task :> Task)
            state.FireEvent OnConnected
            setModel {model with State = Some state}
        <| runner <| (model, cmd)

let private doSend msg ((pkt, callback) : 'pkt * Callback<SendStats>) : Operate<IAgent, Model<'pkt>, Msg<'pkt>> =
    fun runner (model, cmd) ->
        BaseLogic.doSend runner OnSent model.State msg (pkt, callback)
        (model, cmd)

let private handleReq msg req : Operate<IAgent, Model<'pkt>, Msg<'pkt>> =
    fun runner (model, cmd) ->
        match req with
        | DoConnect (a, b, c, d) -> doConnect msg (a, b, c, d)
        | DoSend (a, b) -> doSend msg (a, b)
        <| runner <| (model, cmd)

let private handleEvt _msg evt : Operate<IAgent, Model<'pkt>, Msg<'pkt>> =
    fun runner (model, cmd) ->
        match evt with
        | OnConnected -> 
            noOperation
        | OnDisconnected ->
            setModel {model with State = None}
        | _ -> noOperation
        <| runner <| (model, cmd)

let private update : Update<IAgent, Model<'pkt>, Msg<'pkt>> =
    fun runner model msg -> 
        match msg with
        | WebSocketReq req -> handleReq msg req
        | WebSocketEvt evt -> handleEvt msg evt
        <| runner <| (model, [])

let private init : Init<IAgent,Args<'pkt>, Model<'pkt>, Msg<'pkt>> =
    fun _runner args ->
        ({
            Args = args
            State = None
        }, Cmd.none)

let private subscribe (runner : IAgent) (model : Model<'pkt>) : Cmd<Msg<'pkt>> =
    subscribeEvent runner model WebSocketEvt model.Args.OnEvent

let logic : Logic<IAgent, Args<'pkt>, Model<'pkt>, Msg<'pkt>> = {
    Init = init
    Update = update
    Subscribe = subscribe
}

let getSpec (newArgs : NewArgs<Args<'pkt>>) : AgentSpec<Args<'pkt>, Model<'pkt>, Msg<'pkt>, Req<'pkt>, Evt<'pkt>> =
    {
        Actor =
            {
                NewArgs = newArgs
                Logic = logic
                WrapReq = WebSocketReq
                GetOnEvent = fun model -> model.Args.OnEvent
            }
        OnAgentEvent = None
        GetSlowCap = Some <| getRemoteSlowCap DefaultWebSocketReplySlowCap
    }

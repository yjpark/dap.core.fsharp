[<RequireQualifiedAccess>]
module Dap.Remote.WebSocketService.Logic

open System
open System.Net.WebSockets
open Elmish
open Dap.Prelude
open Dap.Platform
open Dap.Remote
open Dap.Remote.WebSocketService.Types
open Dap.Remote.WebSocketService.Tasks
module WebSocket = Dap.WebSocket.Conn.Types

let private handleService (msg : Service.Msg) : Operate<IAgent, Model<'req, 'evt>, Msg<'req, 'evt>> = 
    fun _runner (model, cmd) ->
        let service = Service.handle msg model.Service
        ({model with Service = service}, cmd)

let private handlerSocketEvt (evt : WebSocket.Evt<Packet'>) : Operate<IAgent, Model<'req, 'evt>, Msg<'req, 'evt>> =
    match evt with 
    | WebSocket.OnReceived (_stats, pkt) ->
        handleService <| Service.OnReceived pkt
    | WebSocket.OnSent (_stats, pkt) ->
        handleService <| Service.OnSent pkt
    | _ ->
        noOperation

let private handleHubEvt (evt : 'evt) : Operate<IAgent, Model<'req, 'evt>, Msg<'req, 'evt>> =
    handleService <| Service.DoSendEvent evt

let private handleInternalEvt (evt : InternalEvt<'evt>) : Operate<IAgent, Model<'req, 'evt>, Msg<'req, 'evt>> =
    match evt with 
    | HubEvt evt ->
        handleHubEvt evt
    | SocketEvt evt ->
        handlerSocketEvt evt
    | OnHandled (packetId, res) ->
        handleService <| Service.DoSendResponse (packetId, res)

let private handleReq msg (req : Req) : Operate<IAgent, Model<'req, 'evt>, Msg<'req, 'evt>> =
    fun runner (model, cmd) ->
        match req with
        | DoAttach (token, socket, callback) ->
            let ident = runner.Ident.Key
            replyAsync runner msg callback nakOnFailed <| doAttachAsync model.State ident token socket
        (model, cmd)

let private update : Update<IAgent, Model<'req, 'evt>, Msg<'req, 'evt>> =
    fun runner model msg -> 
        (match msg with
        | InternalEvt evt -> handleInternalEvt evt
        | ServiceReq req -> handleReq msg req
        )<| runner <| (model, Cmd.none)

let private onRequest runner (state : State<'req, 'evt>) (pkt : Packet') : unit =
    match state.Hub with
    | None ->
        logError runner "onRequest" "Hub_Is_None" pkt
    | Some hub ->
        let callback = fun res ->
            state.Args.FireInternalEvent' <| OnHandled (pkt.Id, res)
        let req = state.Args.HubSpec.DecodeReq pkt.Kind pkt.Payload callback
        hub.PostReq req

let private setHub (state : State<'req, 'evt>) hub =
    state.Hub <- Some hub
    hub.OnEvent.Add(state.Args.FireInternalEvent' << HubEvt)

let private doSend runner (state : State<'req, 'evt>) pkt =
    match state.Socket with
    | None ->
        logError runner "doSend" "Socket_Is_None" pkt
        Some <| SendFailed "Socket_Is_None"
    | Some socket ->
        socket.Post <| WebSocket.DoSend (pkt, None)
        None
    
let private init : Init<IAgent, Args<'req, 'evt>, Model<'req, 'evt>, Msg<'req, 'evt>> =
    fun runner args ->
        let state = {
            Args = args
            Hub = None
            Socket = None
        }
        args.HubSpec.GetHub runner.Ident.Key <| setHub state
        runner.RunTask' ignoreOnFailed' <| setSocketAsync state
        let link : Service.Link = {
            Send = doSend runner state
        }
        let hub' : Service.Hub = {
            OnRequest = onRequest runner state
        }

        let serviceArgs : Service.Args = {
            Link = link
            Hub = hub'
            Logger = runner
            LogTraffic = args.LogTraffic
        }
        ({
            Args = args
            Service = Service.create serviceArgs
            State = state
        }, Cmd.none)

let private subscribe : Subscribe<IAgent, Model<'req, 'evt>, Msg<'req, 'evt>> =
    fun runner model ->
        Cmd.batch [
            subscribeEvent runner model InternalEvt model.Args.OnInternalEvent
        ]

let logic : Logic<IAgent, Args<'req, 'evt>, Model<'req, 'evt>, Msg<'req, 'evt>> =
    {
        Init = init
        Update = update
        Subscribe = subscribe
    }

let getSpec (newArgs : IOwner -> Args<'req, 'evt>) : AgentSpec<Args<'req, 'evt>, Model<'req, 'evt>, Msg<'req, 'evt>, Req, NoEvt> =
    {
        Actor =
            {
                NewArgs = newArgs
                Logic = logic
                WrapReq = ServiceReq
                GetOnEvent = fun _model -> noEvent
            }
        OnAgentEvent = None
        GetSlowCap = None
    }


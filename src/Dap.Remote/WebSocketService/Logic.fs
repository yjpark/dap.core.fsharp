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
module WebSocket = Dap.WebSocket.Types
module WebSocketConn = Dap.WebSocket.Conn.Types

type ActorOperate<'req, 'evt> = ActorOperate<Args<'req, 'evt>, Model<'req, 'evt>, Msg<'req, 'evt>, Req, NoEvt>

let private handleService (msg : Service.Msg) : ActorOperate<'req, 'evt> =
    fun _runner (model, cmd) ->
        let service = model.Service |> Option.get
        let service = Service.handle msg service
        ({model with Service = Some service}, cmd)

let private handlerSocketEvt (evt : WebSocket.Evt<Packet'>) : ActorOperate<'req, 'evt> =
    match evt with
    | WebSocket.OnReceived (_stats, pkt) ->
        handleService <| Service.OnReceived pkt
    | WebSocket.OnSent (_stats, pkt) ->
        handleService <| Service.OnSent pkt
    | WebSocket.OnDisconnected ->
        fun _runner (model, msg) ->
            model.Hub
            |> Option.map (fun hub ->
                hub.OnDisconnected ()
            )
            |> ignore
            (model, msg)
    | _ ->
        noOperation

let private handleHubEvt (evt : 'evt) : ActorOperate<'req, 'evt> =
    handleService <| Service.DoSendEvent evt

let private setHub hub : ActorOperate<'req, 'evt> =
    fun runner (model, cmd) ->
        match model.Hub with
        | None ->
            runner.RunTask3 ignoreOnFailed setSocketAsync
            hub.OnEvent.AddWatcher runner "HubEvt" (runner.Actor.Args.FireInternalEvent' << HubEvt)
            ({model with Hub = Some hub}, Cmd.none)
        | Some hub' ->
            logError runner "WebSocketService" "Hub_Exist" (hub', hub)
            (model, cmd)

let private onRequest (runner : Agent<'req, 'evt>) (pkt : Packet') : unit =
    match runner.Actor.State.Hub with
    | None ->
        logError runner "onRequest" "Hub_Is_None" pkt
    | Some hub ->
        let callback = fun res ->
            runner.Actor.Args.FireInternalEvent' <| OnHandled (pkt.Id, res)
        let req = runner.Actor.Args.HubSpec.DecodeReq pkt.Kind pkt.Payload callback
        hub.PostReq req

let private doSend (runner : Agent<'req, 'evt>) pkt =
    match runner.Actor.State.Socket with
    | None ->
        logError runner "doSend" "Socket_Is_None" pkt
        Some <| SendFailed "Socket_Is_None"
    | Some socket ->
        socket.Post <| WebSocketConn.DoSend (pkt, None)
        None

let private setSocket (socket : PacketConn.Agent) : ActorOperate<'req, 'evt> =
    fun runner (model, cmd) ->
        match model.Socket with
        | None ->
            socket.Actor.OnEvent.AddWatcher runner "SocketEvt" (runner.Actor.Args.FireInternalEvent' << SocketEvt)
            let link : Service.Link = {
                Send = doSend runner
            }
            let hub' : Service.Hub = {
                OnRequest = onRequest runner
            }
            let serviceArgs : Service.Args = {
                Link = link
                Hub = hub'
                Logger = runner
                LogTraffic = runner.Actor.Args.LogTraffic
            }
            let service = Service.create serviceArgs
            ({model with Socket = Some socket ; Service = Some service}, Cmd.none)
        | Some socket' ->
            logError runner "WebSocketService" "Socket_Exist" (socket', socket)
            (model, cmd)

let private handleInternalEvt (evt : InternalEvt<'req, 'evt>) : ActorOperate<'req, 'evt> =
    match evt with
    | SetHub hub ->
        setHub hub
    | SetSocket socket ->
        setSocket socket
    | HubEvt evt ->
        handleHubEvt evt
    | SocketEvt evt ->
        handlerSocketEvt evt
    | OnHandled (packetId, res) ->
        handleService <| Service.DoSendResponse (packetId, res)

let private handleReq msg (req : Req) : ActorOperate<'req, 'evt> =
    fun runner (model, cmd) ->
        match req with
        | DoAttach (token, socket, callback) ->
            let ident = runner.Ident.Key
            replyAsync3 runner msg callback nakOnFailed <| doAttachAsync ident token socket
        (model, cmd)

let private update : ActorUpdate<Args<'req, 'evt>, Model<'req, 'evt>, Msg<'req, 'evt>, Req, NoEvt> =
    fun runner model msg ->
        (match msg with
        | InternalEvt evt -> handleInternalEvt evt
        | ServiceReq req -> handleReq msg req
        )<| runner <| (model, Cmd.none)

let private init : ActorInit<Args<'req, 'evt>, Model<'req, 'evt>, Msg<'req, 'evt>, Req, NoEvt> =
    fun runner args ->
        args.HubSpec.GetHub runner.Ident.Key (args.FireInternalEvent' << SetHub) 
        ({
            Hub = None
            Socket = None
            Service = None
        }, Cmd.none)

let private subscribe : ActorSubscribe<Args<'req, 'evt>, Model<'req, 'evt>, Msg<'req, 'evt>, Req, NoEvt> =
    fun runner model ->
        Cmd.batch [
            subscribeEvent runner model InternalEvt runner.Actor.Args.OnInternalEvent
        ]

let logic =
    {
        Init = init
        Update = update
        Subscribe = subscribe
    }

let getSpec (newArgs : NewArgs<Args<'req, 'evt>>) : AgentSpec<Args<'req, 'evt>, Model<'req, 'evt>, Msg<'req, 'evt>, Req, NoEvt> =
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


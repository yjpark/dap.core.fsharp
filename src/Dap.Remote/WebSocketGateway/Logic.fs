[<RequireQualifiedAccess>]
module Dap.Remote.WebSocketGateway.Logic

open Dap.Prelude
open Dap.Context
open Dap.Platform
open Dap.Remote
open Dap.Remote.Internal
open Dap.Remote.WebSocketGateway.Types
open Dap.Remote.WebSocketGateway.Tasks
module WebSocket = Dap.WebSocket.Types
module WebSocketConn = Dap.WebSocket.Conn.Types

type ActorOperate<'req, 'evt> when 'req :> IReq and 'evt :> IEvt =
    Operate<Agent<'req, 'evt>, Model<'req, 'evt>, Msg<'req, 'evt>>

let private handleGateway (msg : Gateway.Msg) : ActorOperate<'req, 'evt> =
    fun _runner (model, cmd) ->
        let service = model.Gateway |> Option.get
        let service = Gateway.handle msg service
        ({model with Gateway = Some service}, cmd)

let private handlerSocketEvt (evt : WebSocket.Evt<Packet>) : ActorOperate<'req, 'evt> =
    match evt with
    | WebSocket.OnReceived pkt ->
        handleGateway <| Gateway.OnReceived pkt
    | WebSocket.OnSent pkt ->
        handleGateway <| Gateway.OnSent pkt
    | WebSocket.OnStatusChanged status ->
        fun _runner (model, msg) ->
            model.StatusEvent.Trigger status
            (model, msg)

let private handleHubEvt (evt : 'evt) : ActorOperate<'req, 'evt> =
    handleGateway <| Gateway.DoSendEvent evt

let private doInit : ActorOperate<'req, 'evt> =
    fun runner (model, cmd) ->
        runner.Actor.Args.HubSpec.GetHub runner.AsGateway (runner.Deliver << InternalEvt << SetHub)
        (model, cmd)

let private setHub (hub : Hub<'req, 'evt>) : ActorOperate<'req, 'evt> =
    fun runner (model, cmd) ->
        match model.Hub with
        | None ->
            runner.AddTask ignoreOnFailed setSocketAsync
            hub.OnEvent.AddWatcher runner "HubEvt" (runner.Deliver << InternalEvt << HubEvt)
            ({model with Hub = Some hub}, noCmd)
        | Some hub' ->
            logError runner "WebSocketGateway" "Hub_Exist" (hub', hub)
            (model, cmd)

let private onRequest (runner : Agent<'req, 'evt>) ((id, payload) : PacketId * Json) : unit =
    match runner.Actor.State.Hub with
    | None ->
        logError runner "onRequest" "Hub_Is_None" payload
    | Some hub ->
        let onHandled = fun res ->
            runner.Deliver <| InternalEvt ^<| OnHandled (id, res)
        let req = runner.Actor.Args.HubSpec.DecodeRequest runner payload onHandled
        hub.PostReq req

let private doSend (runner : Agent<'req, 'evt>) pkt =
    match runner.Actor.State.Socket with
    | None ->
        logError runner "doSend" "Socket_Is_None" pkt
        Some <| SendFailed "Socket_Is_None"
    | Some socket ->
        socket.Post <| WebSocketConn.DoSend pkt None
        None

let private setSocket (socket : PacketConn.Agent) : ActorOperate<'req, 'evt> =
    fun runner (model, cmd) ->
        match model.Socket with
        | None ->
            socket.Actor.OnEvent.AddWatcher runner "SocketEvt" (runner.Deliver << InternalEvt << SocketEvt)
            let link : Gateway.Link = {
                Send = doSend runner
            }
            let hub' : Gateway.Hub = {
                OnRequest = onRequest runner
            }
            let gatewayArgs : Gateway.Args = {
                Link = link
                Hub = hub'
                Logger = runner
                LogTraffic = runner.Actor.Args.LogTraffic
            }
            let gateway = Gateway.create gatewayArgs
            ({model with Socket = Some socket ; Gateway = Some gateway}, noCmd)
        | Some socket' ->
            logError runner "WebSocketGateway" "Socket_Exist" (socket', socket)
            (model, cmd)

let private handleInternalEvt (evt : InternalEvt<'req, 'evt>) : ActorOperate<'req, 'evt> =
    match evt with
    | DoInit ->
        doInit
    | SetHub hub ->
        setHub hub
    | SetSocket socket ->
        setSocket socket
    | HubEvt evt ->
        handleHubEvt evt
    | SocketEvt evt ->
        handlerSocketEvt evt
    | OnHandled (packetId, res) ->
        handleGateway <| Gateway.DoSendResponse (packetId, res)

let private handleReq (req : Req) : ActorOperate<'req, 'evt> =
    fun runner (model, cmd) ->
        match req with
        | DoAttach (token, socket, callback) ->
            let ident = runner.Ident.Key
            replyAsync runner req callback nakOnFailed <| doAttachAsync ident token socket
        (model, cmd)

let private update : Update<Agent<'req, 'evt>, Model<'req, 'evt>, Msg<'req, 'evt>> =
    fun runner msg model ->
        (match msg with
        | InternalEvt evt -> handleInternalEvt evt
        | GatewayReq req -> handleReq req
        )<| runner <| (model, noCmd)

let private init : ActorInit<Args<'req, 'evt>, Model<'req, 'evt>, Msg<'req, 'evt>> =
    fun runner _args ->
        ({
            Hub = None
            Socket = None
            Gateway = None
            StatusEvent = new Bus<LinkStatus> (runner :> IOwner, "OnStatus")
        }, cmdOfMsg (InternalEvt DoInit))

let logic =
    {
        Init = init
        Update = update
        Subscribe = noSubscription
    }

let spec (args : Args<'req, 'evt>) =
    new ActorSpec<Agent<'req, 'evt>, Args<'req, 'evt>, Model<'req, 'evt>, Msg<'req, 'evt>, Req, NoEvt>
        (Agent<'req, 'evt>.Spawn, args, GatewayReq, noCastEvt, init, update)

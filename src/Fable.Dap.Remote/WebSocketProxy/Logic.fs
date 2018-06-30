[<RequireQualifiedAccess>]
module Dap.Remote.WebSocketProxy.Logic

open System
open Elmish
open Dap.Prelude
open Dap.Platform
open Dap.Remote
open Dap.Remote.WebSocketProxy.Types

module WebSocket = Dap.WebSocket.Client.Types
module WebSocketActor = Dap.WebSocket.Client.Actor

type ActorOperate<'req, 'res, 'evt> =
    ActorOperate<Args<'res, 'evt>, Model<'res, 'evt>, Msg<'req, 'res, 'evt>, 'req, 'evt>

let private handleClient (msg : Client.Msg) : ActorOperate<'req, 'res, 'evt> =
    fun _runner (model, cmd) ->
        let client = Client.handle msg model.Client
        ({model with Client = client}, cmd)

let private doEnqueue (req, pkt) (model : Model<'res, 'evt>) =
    let sendQueue = model.SendQueue |> List.append [(req, pkt)]
    {model with SendQueue = sendQueue}

let private doReconnect : ActorOperate<'req, 'res, 'evt> =
    fun _runner (model, cmd) ->
        model.Socket.Actor.Handle WebSocket.DoConnect
        (model, cmd)

let private doSend' (runner : IAgent) (args : Args<'res, 'evt>) (socket : WebSocket.Agent<Packet'>)
                   ((req, pkt) : IRequest * Packet') : unit =
    let onAck = fun res ->
        args.FireInternalEvent' <| OnSent ^<| (req, pkt, Ok res)
    let onNak = fun (err, detail) ->
        logError runner "Send" "Link_Failed" (req, err, detail)
        args.FireInternalEvent' <| OnSent ^<| (req, pkt, Error <| SendFailed err)
    socket.Actor.Handle <| WebSocket.DoSend (pkt, callback' runner onAck onNak)

let private doSendQueue : ActorOperate<'req, 'res, 'evt> =
    fun runner (model, cmd) ->
        model.SendQueue
        |> List.iter ^<| doSend' runner runner.Actor.Args model.Socket
        ({model with SendQueue = []}, cmd)

let private handleInternalEvt (evt : InternalEvt) : ActorOperate<'req, 'res, 'evt> =
    fun runner (model, cmd) ->
        match evt with
        | OnSent (req, pkt, res) ->
            handleClient <| Client.OnSent (req, pkt, res)
        | DoEnqueue (req, pkt) ->
            updateModel <| doEnqueue (req, pkt)
        | DoReconnect ->
            doReconnect
        <| runner <| (model, Cmd.none)

let private handleProxyReq (req : 'req) : ActorOperate<'req, 'res, 'evt> =
    handleClient <| Client.DoSend req

let private handlerSocketEvt (evt : WebSocket.Evt<Packet'>) : ActorOperate<'req, 'res, 'evt> =
    fun runner (model, cmd) ->
        match evt with
        | WebSocket.OnConnected ->
            doSendQueue
        | WebSocket.OnDisconnected ->
            addFutureCmd 1.0 <| InternalEvt DoReconnect
        | WebSocket.OnReceived pkt ->
            handleClient <| Client.OnReceived pkt
        | _ ->
            noOperation
        <| runner <| (model, Cmd.none)

let private update : ActorUpdate<Args<'res, 'evt>, Model<'res, 'evt>, Msg<'req, 'res, 'evt>, 'req, 'evt> =
    fun runner model msg ->
        (match msg with
        | InternalEvt evt -> handleInternalEvt evt
        | ProxyReq req -> handleProxyReq req
        | ProxyRes _res -> noOperation
        | ProxyEvt _evt -> noOperation
        | SocketEvt evt -> handlerSocketEvt evt
        )<| runner <| (model, Cmd.none)

let private doEnqueue' (runner : IAgent) (args : Args<'res, 'evt>) ((req, pkt) : IRequest * Packet') : unit =
    args.FireInternalEvent' <| DoEnqueue ^<| (req, pkt)

let private doSend (runner : IAgent) (args : Args<'res, 'evt>) (socket : WebSocket.Agent<Packet'>)
                   ((req, pkt) : IRequest * Packet') : LocalReason option =
    match socket.Actor.State.Connected with
    | false ->
        doEnqueue' runner args (req, pkt)
    | true ->
        doSend' runner args socket (req, pkt)
    None

let private onResponse (args : Args<'res, 'evt>) ((req, res) : IRequest * Result<string, Reason'>) : unit =
    match res with
    | Ok json ->
        args.Spec.DecodeRes req json
    | Error reason ->
        match reason with
        | Local' reason ->
            args.Spec.LocalErr req reason
        | Remote' reason ->
            args.Spec.RemoteErr req reason
    |> args.FireResponse'

let private onEvent (args : Args<'res, 'evt>) ((kind, json) : PacketKind * string) : unit =
    args.FireEvent' <| args.Spec.DecodeEvt kind json

let private init : ActorInit<Args<'res, 'evt>, Model<'res, 'evt>, Msg<'req, 'res, 'evt>, 'req, 'evt> =
    fun runner args ->
        let socket = WebSocketActor.create Packet.encode Packet.decode runner.Ident.Key args.Uri false
        let link : Client.Link = {
            Send = doSend runner args socket
        }
        let stub : Client.Stub = {
            OnResponse = onResponse args
            OnEvent = onEvent args
        }
        let clientArgs : Client.Args = {
            Link = link
            Stub = stub
            Logger = runner :> ILogger
            LogTraffic = args.LogTraffic
        }
        ({
            Socket = socket
            Client = Client.create clientArgs
            SendQueue = []
        }, Cmd.none)

let private subscribe : ActorSubscribe<Args<'res, 'evt>, Model<'res, 'evt>, Msg<'req, 'res, 'evt>, 'req, 'evt> =
    fun runner model ->
        Cmd.batch [
            subscribeEvent runner model ProxyEvt runner.Actor.Args.OnEvent
            subscribeEvent runner model ProxyRes runner.Actor.Args.OnResponse
            subscribeEvent runner model InternalEvt runner.Actor.Args.OnInternalEvent
            subscribeEvent runner model SocketEvt model.Socket.Actor.OnEvent
        ]

let logic =
    {
        Init = init
        Update = update
        Subscribe = subscribe
    }

let getSpec (newArgs : NewArgs<Args<'res, 'evt>>) : ActorSpec<Args<'res, 'evt>, Model<'res, 'evt>, Msg<'req, 'res, 'evt>, 'req, 'evt> =
    {
        Logic = logic
        NewArgs = newArgs
        WrapReq = ProxyReq
        GetOnEvent = fun args -> args.OnEvent
    }


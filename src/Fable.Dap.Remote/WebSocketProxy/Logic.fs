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

let private handleClient (msg : Client.Msg) : Operate<IActor, Model<'res, 'evt>, Msg<'req, 'res, 'evt>> = 
    fun _runner (model, cmd) ->
        let client = Client.handle msg model.Client
        ({model with Client = client}, cmd)

let private doEnqueue (req, pkt) (model : Model<'res, 'evt>) =
    let sendQueue = model.SendQueue |> List.append [(req, pkt)]
    {model with SendQueue = sendQueue}

let private doReconnect : Operate<IActor, Model<'res, 'evt>, Msg<'req, 'res, 'evt>> =
    fun _runner (model, cmd) ->
        model.Socket.Handle WebSocket.DoConnect
        (model, cmd)

let private doSend' (runner : IActor) (args : Args<'res, 'evt>) (socket : WebSocket.Actor<Packet'>)
                   ((req, pkt) : IRequest * Packet') : unit =
    let onAck = fun res ->
        args.FireInternalEvent' <| OnSent ^<| (req, pkt, Ok res)
    let onNak = fun (err, detail) ->
        logError runner "Send" "Link_Failed" (req, err, detail)
        args.FireInternalEvent' <| OnSent ^<| (req, pkt, Error <| SendFailed err)
    socket.Handle <| WebSocket.DoSend (pkt, callback' runner onAck onNak)

let private doSendQueue : Operate<IActor, Model<'res, 'evt>, Msg<'req, 'res, 'evt>> =
    fun runner (model, cmd) ->
        model.SendQueue
        |> List.iter ^<| doSend' runner model.Args model.Socket
        ({model with SendQueue = []}, cmd)

let private handleInternalEvt (evt : InternalEvt) : Operate<IActor, Model<'res, 'evt>, Msg<'req, 'res, 'evt>> =
    fun runner (model, cmd) ->
        match evt with 
        | OnSent (req, pkt, res) ->
            handleClient <| Client.OnSent (req, pkt, res)
        | DoEnqueue (req, pkt) ->
            updateModel <| doEnqueue (req, pkt) 
        | DoReconnect ->
            doReconnect
        <| runner <| (model, Cmd.none)

let private handleProxyReq (req : 'req) : Operate<IActor, Model<'res, 'evt>, Msg<'req, 'res, 'evt>> =
    handleClient <| Client.DoSend req

let private handlerSocketEvt (evt : WebSocket.Evt<Packet'>) : Operate<IActor, Model<'res, 'evt>, Msg<'req, 'res, 'evt>> =
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

let private update : Update<IActor, Model<'res, 'evt>, Msg<'req, 'res, 'evt>> =
    fun runner model msg ->
        (match msg with
        | InternalEvt evt -> handleInternalEvt evt
        | ProxyReq req -> handleProxyReq req
        | ProxyRes _res -> noOperation
        | ProxyEvt _evt -> noOperation
        | SocketEvt evt -> handlerSocketEvt evt
        )<| runner <| (model, Cmd.none)

let private doEnqueue' (runner : IActor) (args : Args<'res, 'evt>) ((req, pkt) : IRequest * Packet') : unit =
    args.FireInternalEvent' <| DoEnqueue ^<| (req, pkt)

let private doSend (runner : IActor) (args : Args<'res, 'evt>) (socket : WebSocket.Actor<Packet'>)
                   ((req, pkt) : IRequest * Packet') : LocalReason option =
    match socket.State with
    | None ->
        doEnqueue' runner args (req, pkt)
    | Some model ->
        match model.Ready with
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

let private init : Init<IActor, Args<'res, 'evt>, Model<'res, 'evt>, Msg<'req, 'res, 'evt>> =
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
            Args = args
            Socket = socket
            Client = Client.create clientArgs
            SendQueue = []
        }, Cmd.none)

let private subscribe : Subscribe<IActor, Model<'res, 'evt>, Msg<'req, 'res, 'evt>> =
    fun runner model ->
        Cmd.batch [
            subscribeEvent runner model ProxyEvt model.Args.OnEvent
            subscribeEvent runner model ProxyRes model.Args.OnResponse
            subscribeEvent runner model InternalEvt model.Args.OnInternalEvent
            subscribeEvent runner model SocketEvt model.Socket.OnEvent
        ]

let logic : Logic<IActor, Args<'res, 'evt>, Model<'res, 'evt>, Msg<'req, 'res, 'evt>> =
    {
        Init = init
        Update = update
        Subscribe = subscribe
    }

let getSpec (newArgs : unit -> Args<'res, 'evt>) : ActorSpec<IActor, Args<'res, 'evt>, Model<'res, 'evt>, Msg<'req, 'res, 'evt>, 'req, 'evt> =
    {
        Logic = logic
        NewArgs = newArgs
        WrapReq = ProxyReq
        GetOnEvent = fun m -> m.Args.OnEvent
    }

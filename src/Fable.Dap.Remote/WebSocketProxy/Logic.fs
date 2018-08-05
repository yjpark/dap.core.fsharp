[<RequireQualifiedAccess>]
module Dap.Remote.WebSocketProxy.Logic

open System
open Fable.Core
open Elmish
module E = Thoth.Json.Encode
module D = Thoth.Json.Decode

open Dap.Prelude
open Dap.Platform
open Dap.Remote
open Dap.Remote.Internal
open Dap.Remote.WebSocketProxy.Types

module WebSocketTypes = Dap.WebSocket.Client.Types
module WebSocketAgent = Dap.WebSocket.Client.Agent

type ActorOperate<'req, 'res, 'evt> when 'req :> IRequest and 'evt :> IEvent =
    ActorOperate<Proxy<'req, 'res, 'evt>, Args<'res, 'evt>, Model<'res, 'evt>, Msg<'req, 'res, 'evt>, 'req, 'evt>

let private handleClient (msg : Client.Msg) : ActorOperate<'req, 'res, 'evt> =
    fun _runner (model, cmd) ->
        let client = model.Client |> Option.get
        let client = Client.handle msg client
        ({model with Client = Some client}, cmd)

let private doEnqueue (req, pkt) (model : Model<'res, 'evt>) =
    let sendQueue = model.SendQueue |> List.append [(req, pkt)]
    {model with SendQueue = sendQueue}

let private doReconnect : ActorOperate<'req, 'res, 'evt> =
    fun _runner (model, cmd) ->
        model.Socket.Actor.Handle WebSocketTypes.DoConnect
        (model, cmd)

let private doSend' (runner : Proxy<'req, 'res, 'evt>)
                   ((req, pkt) : IRequest * Packet) : unit =
    let onAck = fun res ->
        runner.Deliver <| InternalEvt ^<| OnSent ^<| (req, pkt, Ok res)
    let onNak = fun (err, detail) ->
        logError runner "Send" "Link_Failed" (req, err, detail)
        runner.Deliver <| InternalEvt ^<| OnSent ^<| (req, pkt, Error <| SendFailed err)
    runner.Actor.State.Socket.Actor.Handle <| WebSocketTypes.DoSend (pkt, callback' runner onNak onAck)

let private doSendQueue : ActorOperate<'req, 'res, 'evt> =
    fun runner (model, cmd) ->
        model.SendQueue
        |> List.iter ^<| doSend' runner
        ({model with SendQueue = []}, cmd)

let private doEnqueue' (runner : Proxy<'req, 'res, 'evt>) ((req, pkt) : IRequest * Packet) : unit =
    runner.Deliver <| InternalEvt ^<| DoEnqueue ^<| (req, pkt)

let private doSend (runner : Proxy<'req, 'res, 'evt>)
                   ((req, pkt) : IRequest * Packet) : LocalReason option =
    let socket = runner.Actor.State.Socket
    match socket.Actor.State.Connected with
    | false ->
        doEnqueue' runner (req, pkt)
    | true ->
        doSend' runner (req, pkt)
    None

[<PassGenericsAttribute>]
let private onResponse (runner : Proxy<'req, 'res, 'evt>) ((req, res) : IRequest * Result<Json, Reason'>) : unit =
    runner.Actor.Args.Spec.DecodeResponse runner req res
    |> (runner.Deliver << ProxyRes)

[<PassGenericsAttribute>]
let private onEvent (runner : Proxy<'req, 'res, 'evt>) ((_id, evt) : PacketId * Json) : unit =
    runner.Actor.Args.Spec.DecodeEvent runner evt
    |> (runner.Deliver << ProxyEvt)

[<PassGenericsAttribute>]
let private doInit : ActorOperate<'req, 'res, 'evt> =
    fun runner (model, cmd) ->
        let link : Client.Link = {
            Send = doSend runner
        }
        let stub : Client.Stub = {
            OnResponse = onResponse runner
            OnEvent = onEvent runner
        }
        let args = runner.Actor.Args
        let clientArgs : Client.Args = {
            Link = link
            Stub = stub
            Logger = runner :> ILogger
            LogTraffic = args.LogTraffic
        }
        let client = Client.create clientArgs
        (runner, model, cmd)
        |=|> updateModel (fun m -> {m with Client = Some client})

[<PassGenericsAttribute>]
let private handleInternalEvt (evt : InternalEvt) : ActorOperate<'req, 'res, 'evt> =
    fun runner (model, cmd) ->
        match evt with
        | DoInit -> doInit
        | OnSent (req, pkt, res) ->
            handleClient <| Client.OnSent (req, pkt, res)
        | DoEnqueue (req, pkt) ->
            updateModel <| doEnqueue (req, pkt)
        | DoReconnect ->
            doReconnect
        <| runner <| (model, noCmd)

[<PassGenericsAttribute>]
let private handleProxyReq (req : 'req) : ActorOperate<'req, 'res, 'evt> =
    handleClient <| Client.DoSend req

[<PassGenericsAttribute>]
let private handlerSocketEvt (evt : WebSocketTypes.Evt<Packet>) : ActorOperate<'req, 'res, 'evt> =
    fun runner (model, cmd) ->
        match evt with
        | WebSocketTypes.OnConnected ->
            doSendQueue
        | WebSocketTypes.OnDisconnected ->
            addFutureCmd 1.0<second> <| InternalEvt DoReconnect
        | WebSocketTypes.OnReceived pkt ->
            handleClient <| Client.OnReceived pkt
        | _ ->
            noOperation
        <| runner <| (model, noCmd)

[<PassGenericsAttribute>]
let private update : ActorUpdate<Proxy<'req, 'res, 'evt>, Args<'res, 'evt>, Model<'res, 'evt>, Msg<'req, 'res, 'evt>, 'req, 'evt> =
    fun runner model msg ->
        (match msg with
        | InternalEvt evt -> handleInternalEvt evt
        | SocketEvt evt -> handlerSocketEvt evt
        | ProxyReq req -> handleProxyReq req
        | ProxyRes res ->
            model.ResponseEvent.Trigger res
            noOperation
        | ProxyEvt _evt -> noOperation
        )<| runner <| (model, noCmd)

let private init : ActorInit<Args<'res, 'evt>, Model<'res, 'evt>, Msg<'req, 'res, 'evt>> =
    fun runner args ->
        let encode = fun (pkt : Packet) -> box (pkt.EncodeJson 0)
        let decode = fun (json : obj) ->
            match json with
            | :? string as pkt ->
                decodeJson Packet.JsonDecoder pkt
            | _ ->
                castJson Packet.JsonDecoder json
        let args = WebSocketAgent.Args<Packet>.Create encode decode args.Uri false
        let socket = runner.Env |> WebSocketAgent.spawn runner.Ident.Key args :?> WebSocketTypes.Agent<Packet>
        ({
            Socket = socket
            Client = None
            SendQueue = []
            ResponseEvent = new Bus<'res> (runner :> IOwner)
        }, Cmd.ofMsg (InternalEvt DoInit))

let private subscribe : ActorSubscribe<Proxy<'req, 'res, 'evt>, Args<'res, 'evt>, Model<'res, 'evt>, Msg<'req, 'res, 'evt>, 'req, 'evt> =
    fun runner model ->
        subscribeBus runner model SocketEvt model.Socket.Actor.OnEvent

[<PassGenericsAttribute>]
let spec<'req, 'res, 'evt when 'req :> IRequest and 'evt :> IEvent> (args : Args<'res, 'evt>) =
    (new ActorSpec<Proxy<'req, 'res, 'evt>, Args<'res, 'evt>, Model<'res, 'evt>, Msg<'req, 'res, 'evt>, 'req, 'evt>
        (Proxy<'req, 'res, 'evt>.Spawn, args, ProxyReq, castEvt<'req, 'res, 'evt>, init, update)
    ).WithSubscribe subscribe


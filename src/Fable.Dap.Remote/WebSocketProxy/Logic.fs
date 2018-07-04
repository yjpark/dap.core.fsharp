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

type ActorOperate<'req, 'res, 'evt> when 'req :> IRequest and 'evt :> IEvent =
    ActorOperate<Args<'res, 'evt>, Model<'res, 'evt>, Msg<'req, 'res, 'evt>, 'req, 'evt>

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
        model.Socket.Actor.Handle WebSocket.DoConnect
        (model, cmd)

let private doSend' (runner : Agent<'req, 'res, 'evt>)
                   ((req, pkt) : IRequest * Packet') : unit =
    let onAck = fun res ->
        runner.Deliver <| InternalEvt ^<| OnSent ^<| (req, pkt, Ok res)
    let onNak = fun (err, detail) ->
        logError runner "Send" "Link_Failed" (req, err, detail)
        runner.Deliver <| InternalEvt ^<| OnSent ^<| (req, pkt, Error <| SendFailed err)
    runner.Actor.State.Socket.Actor.Handle <| WebSocket.DoSend (pkt, callback' runner onAck onNak)

let private doSendQueue : ActorOperate<'req, 'res, 'evt> =
    fun runner (model, cmd) ->
        model.SendQueue
        |> List.iter ^<| doSend' runner
        ({model with SendQueue = []}, cmd)

let private doEnqueue' (runner : Agent<'req, 'res, 'evt>) ((req, pkt) : IRequest * Packet') : unit =
    runner.Deliver <| InternalEvt ^<| DoEnqueue ^<| (req, pkt)

let private doSend (runner : Agent<'req, 'res, 'evt>)
                   ((req, pkt) : IRequest * Packet') : LocalReason option =
    let socket = runner.Actor.State.Socket
    match socket.Actor.State.Connected with
    | false ->
        doEnqueue' runner (req, pkt)
    | true ->
        doSend' runner (req, pkt)
    None

let private onResponse (runner : Agent<'req, 'res, 'evt>) ((req, res) : IRequest * Result<string, Reason'>) : unit =
    let args = runner.Actor.Args
    match res with
    | Ok json ->
        args.Spec.DecodeRes req json
    | Error reason ->
        match reason with
        | Local' reason ->
            args.Spec.LocalErr req reason
        | Remote' reason ->
            args.Spec.RemoteErr req reason
    |> (runner.Deliver << ProxyRes)

let private onEvent (runner : Agent<'req, 'res, 'evt>) ((kind, json) : PacketKind * string) : unit =
    runner.Deliver <| ProxyEvt ^<| runner.Actor.Args.Spec.DecodeEvt kind json

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
        <| runner <| (model, noCmd)

let private update : ActorUpdate<Args<'res, 'evt>, Model<'res, 'evt>, Msg<'req, 'res, 'evt>, 'req, 'evt> =
    fun runner model msg ->
        (match msg with
        | InternalEvt evt -> handleInternalEvt evt
        | SocketEvt evt -> handlerSocketEvt evt
        | ProxyReq req -> handleProxyReq req
        | ProxyRes res ->
            runner.Actor.Args.ResponseEvent'.Trigger res
            noOperation
        | ProxyEvt _evt -> noOperation
        )<| runner <| (model, noCmd)

let private init : ActorInit<Args<'res, 'evt>, Model<'res, 'evt>, Msg<'req, 'res, 'evt>> =
    fun runner args ->
        let socket = WebSocketActor.create' Packet.encode Packet.decode runner.Ident.Key args.Uri false
        ({
            Socket = socket
            Client = None
            SendQueue = []
        }, Cmd.ofMsg (InternalEvt DoInit))

let private subscribe : ActorSubscribe<Args<'res, 'evt>, Model<'res, 'evt>, Msg<'req, 'res, 'evt>, 'req, 'evt> =
    fun runner model ->
        subscribeBus runner model SocketEvt model.Socket.Actor.OnEvent

let logic<'req, 'res, 'evt when 'req :> IRequest and 'evt :> IEvent> : ActorLogic<Args<'res, 'evt>, Model<'res, 'evt>, Msg<'req, 'res, 'evt>, 'req, 'evt>=
    {
        Init = init
        Update = update
        Subscribe = subscribe
    }

let getSpec<'req, 'res, 'evt when 'req :> IRequest and 'evt :> IEvent> (newArgs : ActorNewArgs<Args<'res, 'evt>>) : ActorSpec<Args<'res, 'evt>, Model<'res, 'evt>, Msg<'req, 'res, 'evt>, 'req, 'evt> =
    {
        Logic = logic<'req, 'res, 'evt>
        NewArgs = newArgs
        WrapReq = ProxyReq
        CastEvt = castEvt<'req, 'res, 'evt>
    }


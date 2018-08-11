[<RequireQualifiedAccess>]
module Dap.Remote.Proxy.Logic

open System
#if FABLE_COMPILER
open Fable.Core
#endif

open Elmish

open Dap.Prelude
open Dap.Platform
open Dap.Remote
open Dap.Remote.Internal
open Dap.Remote.Proxy.Types

let handleClient (msg : Client.Msg) : ActorOperate<'extra, 'sub, 'req, 'res, 'evt> =
    fun _runner (model, cmd) ->
        let client = model.Client |> Option.get
        let client = Client.handle msg client
        ({model with Client = Some client}, cmd)

let private doEnqueue (req, pkt) (model : Model<'extra, 'res, 'evt>) =
    let sendQueue = model.SendQueue |> List.append [(req, pkt)]
    {model with SendQueue = sendQueue}

let private doSend' (runner : Proxy<'extra, 'sub, 'req, 'res, 'evt>)
                   ((req, pkt) : IRequest * Packet) : unit =
    let onAck = fun res ->
        runner.Deliver <| InternalEvt ^<| OnSent ^<| (req, pkt, Ok res)
    let onNak = fun (err, detail) ->
        logError runner "Send" "Link_Failed" (req, err, detail)
        runner.Deliver <| InternalEvt ^<| OnSent ^<| (req, pkt, Error <| SendFailed err)
    runner.Actor.Args.Sub.DoSend runner pkt <| callback' runner onNak onAck

let doSendQueue : ActorOperate<'extra, 'sub, 'req, 'res, 'evt> =
    fun runner (model, cmd) ->
        model.SendQueue
        |> List.iter ^<| doSend' runner
        ({model with SendQueue = []}, cmd)

let private doEnqueue' (runner : Proxy<'extra, 'sub, 'req, 'res, 'evt>) ((req, pkt) : IRequest * Packet) : unit =
    runner.Deliver <| InternalEvt ^<| DoEnqueue ^<| (req, pkt)

let private doSend (runner : Proxy<'extra, 'sub, 'req, 'res, 'evt>)
                   ((req, pkt) : IRequest * Packet) : LocalReason option =
    match runner.Actor.Args.Sub.CalcConnected runner.Actor.State.Extra with
    | false ->
        doEnqueue' runner (req, pkt)
    | true ->
        doSend' runner (req, pkt)
    None

#if FABLE_COMPILER
[<PassGenericsAttribute>]
#endif
let private onResponse (runner : Proxy<'extra, 'sub, 'req, 'res, 'evt>) ((req, res) : IRequest * Result<Json, Reason'>) : unit =
    runner.Actor.Args.Spec.DecodeResponse runner req res
    |> (runner.Deliver << ProxyRes)

#if FABLE_COMPILER
[<PassGenericsAttribute>]
#endif
let private onEvent (runner : Proxy<'extra, 'sub, 'req, 'res, 'evt>) ((_id, evt) : PacketId * Json) : unit =
    runner.Actor.Args.Spec.DecodeEvent runner evt
    |> (runner.Deliver << ProxyEvt)

#if FABLE_COMPILER
[<PassGenericsAttribute>]
#endif
let private doInit : ActorOperate<'extra, 'sub, 'req, 'res, 'evt> =
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
        |-|> updateModel (fun m -> {m with Client = Some client})
        |=|> runner.Actor.Args.Sub.DoInit

#if FABLE_COMPILER
[<PassGenericsAttribute>]
#endif
let private handleInternalEvt (evt : InternalEvt) : ActorOperate<'extra, 'sub, 'req, 'res, 'evt> =
    fun runner (model, cmd) ->
        match evt with
        | DoInit -> doInit
        | OnSent (req, pkt, res) ->
            handleClient <| Client.OnSent (req, pkt, res)
        | DoEnqueue (req, pkt) ->
            updateModel <| doEnqueue (req, pkt)
        <| runner <| (model, cmd)

#if FABLE_COMPILER
[<PassGenericsAttribute>]
#endif
let private handleProxyReq (req : 'req) : ActorOperate<'extra, 'sub, 'req, 'res, 'evt> =
    handleClient <| Client.DoSend req

#if FABLE_COMPILER
[<PassGenericsAttribute>]
#endif
let private update : Update<Proxy<'extra, 'sub, 'req, 'res, 'evt>, Model<'extra, 'res, 'evt>, Msg<'sub, 'req, 'res, 'evt>> =
    fun runner msg model ->
        (match msg with
        | InternalEvt evt -> handleInternalEvt evt
        | SubEvt evt -> runner.Actor.Args.Sub.HandleSub evt
        | ProxyReq req -> handleProxyReq req
        | ProxyRes res ->
            model.ResponseEvent.Trigger res
            noOperation
        | ProxyEvt _evt -> noOperation
        )<| runner <| (model, noCmd)

let private init : ActorInit<Args<'extra, 'sub, 'req, 'res, 'evt>, Model<'extra, 'res, 'evt>, Msg<'sub, 'req, 'res, 'evt>> =
    fun runner args ->
        ({
            Client = None
            SendQueue = []
            ResponseEvent = new Bus<'res> (runner :> IOwner)
            Extra = args.Sub.NewExtra ()
        }, Cmd.ofMsg (InternalEvt DoInit))

#if FABLE_COMPILER
[<PassGenericsAttribute>]
#endif
let spec<'extra, 'sub, 'req, 'res, 'evt when 'req :> IRequest and 'evt :> IEvent> (args : Args<'extra, 'sub, 'req, 'res, 'evt>) =
    new ActorSpec<Proxy<'extra, 'sub, 'req, 'res, 'evt>, Args<'extra, 'sub, 'req, 'res, 'evt>, Model<'extra, 'res, 'evt>, Msg<'sub, 'req, 'res, 'evt>, 'req, 'evt>
        (Proxy<'extra, 'sub, 'req, 'res, 'evt>.Spawn, args, ProxyReq, castEvt<'sub, 'req, 'res, 'evt>, init, update)


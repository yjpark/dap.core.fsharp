[<RequireQualifiedAccess>]
module Dap.Remote.Proxy.Logic

open System

open Dap.Prelude
open Dap.Context
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

let doDropQueue : ActorOperate<'extra, 'sub, 'req, 'res, 'evt> =
    fun runner (model, cmd) ->
        ({model with SendQueue = []}, cmd)

let private doEnqueue' (runner : Proxy<'extra, 'sub, 'req, 'res, 'evt>) ((req, pkt) : IRequest * Packet) : unit =
    runner.Deliver <| InternalEvt ^<| DoEnqueue ^<| (req, pkt)

let private doSend (runner : Proxy<'extra, 'sub, 'req, 'res, 'evt>)
                   ((req, pkt) : IRequest * Packet) : LocalReason option =
    match runner.Actor.State.Status with
    | LinkStatus.Linked ->
        doSend' runner (req, pkt)
    | _ ->
        doEnqueue' runner (req, pkt)
    None

let private doInit : ActorOperate<'extra, 'sub, 'req, 'res, 'evt> =
    fun runner (model, cmd) ->
        let link : Client.Link = {
            Send = doSend runner
        }
        let stub : Client.Stub = {
            OnResponse = (fun (req, res) ->
                let res = runner.Actor.Args.Spec.DecodeResponse runner req res
                runner.Deliver <| ProxyRes res
            )
            OnEvent = (fun (_id, evt) ->
                let evt = runner.Actor.Args.Spec.DecodeEvent runner evt
                runner.Deliver <| ProxyEvt evt
            )
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

let private handleInternalEvt (evt : InternalEvt) : ActorOperate<'extra, 'sub, 'req, 'res, 'evt> =
    fun runner (model, cmd) ->
        match evt with
        | DoInit -> doInit
        | OnSent (req, pkt, res) ->
            handleClient <| Client.OnSent (req, pkt, res)
        | DoEnqueue (req, pkt) ->
            updateModel <| doEnqueue (req, pkt)
        | DoSetStatus status ->
            (match status with
            | LinkStatus.Linked ->
                doSendQueue
            | LinkStatus.Closed ->
                doDropQueue
            | _ ->
                noOperation
            )|-|- updateModel (fun m -> {m with Status = status})
            |-|- addSubCmd InternalEvt DoTriggerOnStatus
        | DoTriggerOnStatus ->
            model.StatusEvent.Trigger model.Status
            noOperation
        <| runner <| (model, cmd)

let private handleProxyReq (req : 'req) : ActorOperate<'extra, 'sub, 'req, 'res, 'evt> =
    handleClient <| Client.DoSend req

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
            StatusEvent = new Bus<LinkStatus> (runner :> IOwner, "OnStatus")
            ResponseEvent = new Bus<'res> (runner :> IOwner, "OnResponse")
            Status = LinkStatus.NoLink
            Extra = args.Sub.NewExtra ()
        }, cmdOfMsg (InternalEvt DoInit))

let spec<'extra, 'sub, 'req, 'res, 'evt when 'req :> IRequest and 'evt :> IEvent> (args : Args<'extra, 'sub, 'req, 'res, 'evt>) =
    new ActorSpec<Proxy<'extra, 'sub, 'req, 'res, 'evt>, Args<'extra, 'sub, 'req, 'res, 'evt>, Model<'extra, 'res, 'evt>, Msg<'sub, 'req, 'res, 'evt>, 'req, 'evt>
        (Proxy<'extra, 'sub, 'req, 'res, 'evt>.Spawn, args, ProxyReq, castEvt<'sub, 'req, 'res, 'evt>, init, update)


module Dap.Remote.Proxy.Types

open System
open Dap.Prelude
open Dap.Platform
open Dap.Remote
open Dap.Remote.Internal

type SubSpec<'extra, 'sub, 'req, 'res, 'evt> when 'req :> IRequest and 'evt :> IEvent = {
    NewExtra : unit -> 'extra
    DoInit : ActorOperate<'extra, 'sub, 'req, 'res, 'evt>
    HandleSub : 'sub -> ActorOperate<'extra, 'sub, 'req, 'res, 'evt>
    DoSend : Proxy<'extra, 'sub, 'req, 'res, 'evt> -> Packet -> Callback<DateTime> -> unit
}

and Args<'extra, 'sub, 'req, 'res, 'evt> when 'req :> IRequest and 'evt :> IEvent = {
    Sub : SubSpec<'extra, 'sub, 'req, 'res, 'evt>
    Spec : StubSpec<'req, 'res, 'evt>
    Uri : string
    LogTraffic : bool
} with
    static member Create sub spec uri logTraffic =
        {
            Sub = sub
            Spec = spec
            Uri = uri
            LogTraffic = logTraffic
        }

and InternalEvt =
    | DoInit
    | OnSent of IRequest * Packet * Result<DateTime, LocalReason>
    | DoEnqueue of IRequest * Packet
    | DoSetStatus of LinkStatus
    | DoTriggerOnStatus

and Msg<'sub, 'req, 'res, 'evt> when 'req :> IRequest and 'evt :> IEvent =
    | InternalEvt of InternalEvt
    | SubEvt of 'sub
    | ProxyReq of 'req
    | ProxyRes of 'res
    | ProxyEvt of 'evt
with interface IMsg

and Model<'extra, 'res, 'evt> when 'evt :> IEvent = {
    Client : Client.Model option
    SendQueue : (IRequest * Packet) list
    StatusEvent : Bus<LinkStatus>
    ResponseEvent : Bus<'res>
    Status : LinkStatus
    Extra : 'extra
} with
    member this.WithExtra (extra : 'extra) = {this with Extra = extra}

and ActorOperate<'extra, 'sub, 'req, 'res, 'evt when 'req :> IRequest and 'evt :> IEvent> =
    Operate<Proxy<'extra, 'sub, 'req, 'res, 'evt>, Model<'extra, 'res, 'evt>, Msg<'sub, 'req, 'res, 'evt>>

and Proxy<'extra, 'sub, 'req, 'res, 'evt when 'req :> IRequest and 'evt :> IEvent> (param) =
    inherit BaseAgent<Proxy<'extra, 'sub, 'req, 'res, 'evt>, Args<'extra, 'sub, 'req, 'res, 'evt>, Model<'extra, 'res, 'evt>, Msg<'sub, 'req, 'res, 'evt>, 'req, 'evt> (param)
    static member Spawn (param) = new Proxy<'extra, 'sub, 'req, 'res, 'evt> (param)
    override this.Runner = this
    member this.Status = this.Actor.State.Status
    member this.OnStatus = this.Actor.State.StatusEvent.Publish
    member this.OnResponse = this.Actor.State.ResponseEvent.Publish
    interface IProxy<'req, 'res, 'evt> with
        member this.Status = this.Status
        member this.OnStatus = this.OnStatus
        member this.OnResponse = this.OnResponse

let castEvt<'sub, 'req, 'res, 'evt when 'req :> IRequest and 'evt :> IEvent> : CastEvt<Msg<'sub, 'req, 'res, 'evt>, 'evt> =
    function
    | ProxyEvt evt -> Some evt
    | _ -> None


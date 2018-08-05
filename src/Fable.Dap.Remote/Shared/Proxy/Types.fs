module Dap.Remote.Proxy.Types

open Dap.Prelude
open Dap.Platform
open Dap.Remote
open Dap.Remote.Internal

type SubSpec<'extra, 'sub, 'req, 'res, 'evt> when 'req :> IRequest and 'evt :> IEvent = {
    NewExtra : unit -> 'extra
    DoInit : ActorOperate<'extra, 'sub, 'req, 'res, 'evt>
    CalcConnected : 'extra -> bool
    HandleSub : 'sub -> ActorOperate<'extra, 'sub, 'req, 'res, 'evt>
    DoSend : Proxy<'extra, 'sub, 'req, 'res, 'evt> -> Packet -> Callback<Instant> -> unit
}

and Args<'extra, 'sub, 'req, 'res, 'evt> when 'req :> IRequest and 'evt :> IEvent = {
    Sub : SubSpec<'extra, 'sub, 'req, 'res, 'evt>
    Spec : StubSpec<'res, 'evt>
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
    | OnSent of IRequest * Packet * Result<Instant, LocalReason>
    | DoEnqueue of IRequest * Packet

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
    ResponseEvent : Bus<'res>
    Extra : 'extra
} with
    member this.WithExtra (extra : 'extra) = {this with Extra = extra}

and ActorOperate<'extra, 'sub, 'req, 'res, 'evt when 'req :> IRequest and 'evt :> IEvent> =
    Operate<Proxy<'extra, 'sub, 'req, 'res, 'evt>, Model<'extra, 'res, 'evt>, Msg<'sub, 'req, 'res, 'evt>>

and Proxy<'extra, 'sub, 'req, 'res, 'evt when 'req :> IRequest and 'evt :> IEvent> (param) =
    inherit BaseAgent<Proxy<'extra, 'sub, 'req, 'res, 'evt>, Args<'extra, 'sub, 'req, 'res, 'evt>, Model<'extra, 'res, 'evt>, Msg<'sub, 'req, 'res, 'evt>, 'req, 'evt> (param)
    static member Spawn (param) = new Proxy<'extra, 'sub, 'req, 'res, 'evt> (param)
    override this.Runner = this
    member this.Connected =
        this.Actor.Args.Sub.CalcConnected this.Actor.State.Extra
    member this.OnResponse = this.Actor.State.ResponseEvent.Publish
    interface IProxy<'req, 'res, 'evt> with
        member this.Connected = this.Connected
        member this.OnResponse = this.OnResponse

let castEvt<'sub, 'req, 'res, 'evt when 'req :> IRequest and 'evt :> IEvent> : CastEvt<Msg<'sub, 'req, 'res, 'evt>, 'evt> =
    function
    | ProxyEvt evt -> Some evt
    | _ -> None


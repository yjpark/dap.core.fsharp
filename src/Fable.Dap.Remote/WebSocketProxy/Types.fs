module Dap.Remote.WebSocketProxy.Types

open Dap.Prelude
open Dap.Platform
open Dap.Remote

module WebSocketTypes = Dap.WebSocket.Client.Types

type InternalEvt =
    | DoInit
    | OnSent of IRequest * Packet' * Result<System.DateTime, LocalReason>
    | DoEnqueue of IRequest * Packet'
    | DoReconnect

and Args<'res, 'evt> when 'evt :> IEvent = {
    Spec : StubSpec<'res, 'evt>
    Uri : string
    LogTraffic : bool
} with
    static member Create spec uri logTraffic =
        {
            Spec = spec
            Uri = uri
            LogTraffic = logTraffic
        }

and Msg<'req, 'res, 'evt> when 'req :> IRequest and 'evt :> IEvent =
    | InternalEvt of InternalEvt
    | SocketEvt of WebSocketTypes.Evt<Packet'>
    | ProxyReq of 'req
    | ProxyRes of 'res
    | ProxyEvt of 'evt
with interface IMsg

and Model<'res, 'evt> when 'evt :> IEvent = {
    Socket : WebSocketTypes.Agent<Packet'>
    Client : Client.Model option
    SendQueue : (IRequest * Packet') list
    ResponseEvent : Bus<'res>
} with
    member this.Connected = this.Socket.Actor.State.Connected

let castEvt<'req, 'res, 'evt when 'req :> IRequest and 'evt :> IEvent> : CastEvt<Msg<'req, 'res, 'evt>, 'evt> =
    function
    | ProxyEvt evt -> Some evt
    | _ -> None

type Proxy<'req, 'res, 'evt when 'req :> IRequest and 'evt :> IEvent> (param) =
    inherit BaseAgent<Proxy<'req, 'res, 'evt>, Args<'res, 'evt>, Model<'res, 'evt>, Msg<'req, 'res, 'evt>, 'req, 'evt> (param)
    override this.Runner = this
    static member Spawn (param) = new Proxy<'req, 'res, 'evt> (param)
    member this.Connected = this.Actor.State.Connected
    member this.OnResponse = this.Actor.State.ResponseEvent.Publish
    interface IProxy<'req, 'res, 'evt> with
        member this.Connected = this.Connected
        member this.OnResponse = this.OnResponse

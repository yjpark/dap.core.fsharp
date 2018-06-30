module Dap.Remote.WebSocketProxy.Types

open Dap.Prelude
open Dap.Platform
open Dap.Remote
module WebSocket = Dap.WebSocket.Client.Types

[<Literal>]
let Kind = "WebSocketProxy"

type Agent<'req, 'res, 'evt> when 'req :> IReq and 'evt :> IEvt =
    IAgent<Args<'res, 'evt>, Model<'res, 'evt>, 'req, 'evt>

and InternalEvt =
    | OnSent of IRequest * Packet' * Result<System.DateTime, LocalReason>
    | DoEnqueue of IRequest * Packet'
    | DoReconnect

and Args<'res, 'evt> when 'evt :> IEvt = {
    Spec : StubSpec<'res, 'evt>
    Uri : string
    LogTraffic : bool
    Event' : Bus<'evt>
    ResponseEvent' : Bus<'res>
    InternalEvent' : Bus<InternalEvt>
} with
    member this.FireEvent' = this.Event'.Trigger
    member this.OnEvent = this.Event'.Publish
    member this.FireResponse' = this.ResponseEvent'.Trigger
    member this.OnResponse = this.ResponseEvent'.Publish
    member this.FireInternalEvent' = this.InternalEvent'.Trigger
    member this.OnInternalEvent = this.InternalEvent'.Publish

and Msg<'req, 'res, 'evt> when 'req :> IReq and 'evt :> IEvt =
    | InternalEvt of InternalEvt
    | SocketEvt of WebSocket.Evt<Packet'>
    | ProxyReq of 'req
    | ProxyRes of 'res
    | ProxyEvt of 'evt
with interface IMsg

and Model<'res, 'evt> when 'evt :> IEvt = {
    Socket : WebSocket.Agent<Packet'>
    Client : Client.Model
    SendQueue : (IRequest * Packet') list
} with
    member this.Connected = this.Socket.Actor.State.Connected
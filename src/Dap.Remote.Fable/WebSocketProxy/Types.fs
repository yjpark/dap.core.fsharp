module Dap.Remote.WebSocketProxy.Types

open Dap.Prelude
open Dap.Platform
open Dap.Remote
module WebSocket = Dap.WebSocket.Client.Types

[<Literal>]
let Kind = "WebSocketProxy"

type InternalEvt =
    | OnSent of IRequest * Packet' * Result<System.DateTime, LocalReason>
    | DoEnqueue of IRequest * Packet'
    | DoReconnect

type Args<'res, 'evt> = {
    Spec : StubSpec<'res, 'evt>
    Uri : string
    LogTraffic : bool
    Event' : Event<'evt>
    ResponseEvent' : Event<'res>
    InternalEvent' : Event<InternalEvt>
} with
    member this.FireEvent' = this.Event'.Trigger
    member this.OnEvent = this.Event'.Publish
    member this.FireResponse' = this.ResponseEvent'.Trigger
    member this.OnResponse = this.ResponseEvent'.Publish
    member this.FireInternalEvent' = this.InternalEvent'.Trigger
    member this.OnInternalEvent = this.InternalEvent'.Publish

type Msg<'req, 'res, 'evt> =
    | InternalEvt of InternalEvt
    | SocketEvt of WebSocket.Evt<Packet'>
    | ProxyReq of 'req
    | ProxyRes of 'res
    | ProxyEvt of 'evt
with interface IMsg

type Model<'res, 'evt> = {
    Args : Args<'res, 'evt>
    Socket : WebSocket.Actor<Packet'>
    Client : Client.Model
    SendQueue : (IRequest * Packet') list
}

type Actor<'req, 'res, 'evt> when 'req :> IRequest =
    IActor<Model<'res, 'evt>, 'req, 'evt>
module Dap.Remote.WebSocketProxy.Types

open Dap.Prelude
open Dap.Platform
open Dap.Remote
module WebSocket = Dap.WebSocket.Client.Types

[<Literal>]
let Kind = "WebSocketProxy"

type Agent<'req, 'res, 'evt> when 'req :> IReq and 'evt :> IEvt =
    IAgent<Args<'res, 'evt>, Model<'res, 'evt>, Msg<'req, 'res, 'evt>, 'req, 'evt>

and InternalEvt =
    | DoInit
    | OnSent of IRequest * Packet' * Result<System.DateTime, LocalReason>
    | DoEnqueue of IRequest * Packet'
    | DoReconnect

and Args<'res, 'evt> when 'evt :> IEvt = {
    Spec : StubSpec<'res, 'evt>
    Uri : string
    LogTraffic : bool
    ResponseEvent' : Bus<'res>
} with
    member this.OnResponse = this.ResponseEvent'.Publish

and Msg<'req, 'res, 'evt> when 'req :> IReq and 'evt :> IEvt =
    | InternalEvt of InternalEvt
    | SocketEvt of WebSocket.Evt<Packet'>
    | ProxyReq of 'req
    | ProxyRes of 'res
    | ProxyEvt of 'evt
with interface IMsg

and Model<'res, 'evt> when 'evt :> IEvt = {
    Socket : WebSocket.Agent<Packet'>
    Client : Client.Model option
    SendQueue : (IRequest * Packet') list
} with
    member this.Connected = this.Socket.Actor.State.Connected

let castEvt<'req, 'res, 'evt when 'req :> IReq and 'evt :> IEvt> : CastEvt<Msg<'req, 'res, 'evt>, 'evt> =
    function
    | ProxyEvt evt -> Some evt
    | _ -> None
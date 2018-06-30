module Dap.Remote.WebSocketService.Types

open System.Threading
open System.Threading.Tasks
open System.Net.WebSockets
open Dap.Prelude
open Dap.Remote
open Dap.Platform
module WebSocket = Dap.WebSocket.Types

type Agent<'req, 'evt> = IAgent<Args<'req, 'evt>, Model<'req, 'evt>, Req, NoEvt>

and InternalEvt<'req, 'evt> =
    | SetHub of Hub<'req, 'evt>
    | SetSocket of PacketConn.Agent
    | HubEvt of 'evt
    | SocketEvt of WebSocket.Evt<Packet'>
    | OnHandled of PacketId * Result<IResponse, HubReason>

and Args<'req, 'evt> = {
    HubSpec : HubSpec<'req, 'evt>
    LogTraffic : bool
    InternalEvent' : Bus<InternalEvt<'req, 'evt>>
} with
    member this.FireInternalEvent' = this.InternalEvent'.Trigger
    member this.OnInternalEvent = this.InternalEvent'.Publish

and Model<'req, 'evt> = {
    Hub : Hub<'req, 'evt> option
    Socket : PacketConn.Agent option
    Service : Service.Model option
}

and Req =
    | DoAttach of CancellationToken * WebSocket * Callback<Task>

and Msg<'req, 'evt> =
    | InternalEvt of InternalEvt<'req, 'evt>
    | ServiceReq of Req
with interface IMsg

let DoAttach' (token : CancellationToken) (socket : WebSocket) callback =
    DoAttach (token, socket, callback)

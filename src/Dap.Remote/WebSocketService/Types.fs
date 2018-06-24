module Dap.Remote.WebSocketService.Types

open System.Threading
open System.Threading.Tasks
open System.Net.WebSockets
open Dap.Prelude
open Dap.Remote
open Dap.Platform
module WebSocket = Dap.WebSocket.Conn.Types

type Agent<'req, 'evt> = IAgent<Model<'req, 'evt>, Req, NoEvt>

and InternalEvt<'evt> =
    | HubEvt of 'evt
    | SocketEvt of WebSocket.Evt<Packet'>
    | OnHandled of PacketId * Result<IResponse, HubReason>

and Args<'req, 'evt> = {
    HubSpec : HubSpec<'req, 'evt>
    LogTraffic : bool
    InternalEvent' : Bus<InternalEvt<'evt>>
} with
    member this.FireInternalEvent' = this.InternalEvent'.Trigger
    member this.OnInternalEvent = this.InternalEvent'.Publish

and State<'req, 'evt> = {
    Args : Args<'req, 'evt>
    mutable Hub : Hub<'req, 'evt> option
    mutable Socket : PacketConn.Agent option
}

and Model<'req, 'evt> = {
    Args : Args<'req, 'evt>
    Service : Service.Model
    State : State<'req, 'evt>
}

and Req =
    | DoAttach of CancellationToken * WebSocket * Callback<Task>

and Msg<'req, 'evt> =
    | InternalEvt of InternalEvt<'evt>
    | ServiceReq of Req
with interface IMsg

let DoAttach' (token : CancellationToken) (socket : WebSocket) callback =
    DoAttach (token, socket, callback)

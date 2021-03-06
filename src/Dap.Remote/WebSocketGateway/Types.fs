module Dap.Remote.WebSocketGateway.Types

open System.Threading
open System.Threading.Tasks
open System.Net.WebSockets

open Dap.Prelude
open Dap.Context
open Dap.Remote
open Dap.Remote.Internal
open Dap.Platform
module WebSocket = Dap.WebSocket.Types

type InternalEvt<'req, 'evt> when 'req :> IReq and 'evt :> IEvt =
    | DoInit
    | SetHub of Hub<'req, 'evt>
    | SetSocket of PacketConn.Agent
    | HubEvt of 'evt
    | SocketEvt of WebSocket.Evt<Packet>
    | OnHandled of PacketId * Result<IResult, HubReason>

and Args<'req, 'evt> when 'req :> IReq and 'evt :> IEvt = {
    HubSpec : HubSpec<'req, 'evt>
    LogTraffic : bool
} with
    static member Create hubSpec logTraffic =
        {
            HubSpec = hubSpec
            LogTraffic = logTraffic
        }

and Model<'req, 'evt> when 'req :> IReq and 'evt :> IEvt = {
    Hub : Hub<'req, 'evt> option
    Socket : PacketConn.Agent option
    Gateway : Gateway.Model option
    StatusEvent : Bus<LinkStatus>
}

and Req =
    | DoAttach of CancellationToken * WebSocket * Callback<Task>
with interface IReq

and Msg<'req, 'evt> when 'req :> IReq and 'evt :> IEvt =
    | InternalEvt of InternalEvt<'req, 'evt>
    | GatewayReq of Req
with interface IMsg

let DoAttach (token : CancellationToken) (socket : WebSocket) callback =
    DoAttach (token, socket, callback)

type Agent<'req, 'evt> when 'req :> IReq and 'evt :> IEvt (param) =
    inherit BaseAgent<Agent<'req, 'evt>, Args<'req, 'evt>, Model<'req, 'evt>, Msg<'req, 'evt>, Req, NoEvt> (param)
    override this.Runner = this
    static member Spawn (param) = new Agent<'req, 'evt> (param)
    member this.Status =
        this.Actor.State.Socket
        |> Option.map (fun s -> s.Actor.State.Status)
        |> Option.defaultValue LinkStatus.NoLink
    member this.OnStatus = this.Actor.State.StatusEvent.Publish
    interface IGateway with
        member this.Status = this.Status
        member this.OnStatus = this.OnStatus
    member this.AsGateway = this :> IGateway


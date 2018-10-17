module Dap.WebSocket.Types

open System
open System.Net.WebSockets
open System.Threading

open Dap.Prelude
open Dap.Context
open Dap.Platform

type Encode<'pkt> = 'pkt -> ArraySegment<byte>

// buffer, index, count
type Decode<'pkt> = array<byte> * int * int -> 'pkt

type Args<'socket, 'pkt, 'req> when 'socket :> WebSocket and 'req :> IReq = {
    LogTraffic : bool
    SendType : WebSocketMessageType
    BufferSize : int
    RefreshInterval : Duration
    Encode : Encode<'pkt>
    Decode : Decode<'pkt>
    HandleReq : 'req -> ActorOperate<'socket, 'pkt, 'req>
} with
    static member Create logTraffic sendType bufferSize refreshInterval encode decode handleReq =
        {
            LogTraffic = logTraffic
            SendType = sendType
            BufferSize = bufferSize
            RefreshInterval = refreshInterval
            Encode = encode
            Decode = decode
            HandleReq = handleReq
        }

and Evt<'pkt> =
    | OnSent of 'pkt
    | OnReceived of 'pkt
    | OnStatusChanged of LinkStatus
with interface IEvt

and InternalEvt =
    | OnLinked
    | TryCloseSocket
    | DoRefreshStatus of exn option
    | OnTick of Instant * Duration

and Msg<'pkt, 'req> =
    | WebSocketReq of 'req
    | WebSocketEvt of Evt<'pkt>
    | InternalEvt of InternalEvt
with interface IMsg

and Model<'socket, 'pkt> when 'socket :> WebSocket = {
    Link : Link<'socket> option
    Status : LinkStatus
    NextRefreshTime : Instant
}

and [<StructuredFormatDisplay("<Link>{AsDisplay}")>]
    Link<'socket> when 'socket :> WebSocket = {
    Ident : string
    Token : CancellationToken
    Socket : 'socket
    Buffer : byte[]
} with
    member this.AsDisplay = (this.Ident, this.Socket)

and ActorOperate<'socket, 'pkt, 'req> when 'socket :> WebSocket and 'req :> IReq =
    Operate<Agent<'socket, 'pkt, 'req>, Model<'socket, 'pkt>, Msg<'pkt, 'req>>

and Agent<'socket, 'pkt, 'req> when 'socket :> WebSocket and 'req :> IReq (pack, param) =
    inherit PackAgent<ITickingPack, Agent<'socket, 'pkt, 'req>, Args<'socket, 'pkt, 'req>, Model<'socket, 'pkt>, Msg<'pkt, 'req>, 'req, Evt<'pkt>> (pack, param)
    let linkStats = base.Console.Stats.Target.AddCustom<LinkStats> (LinkStats.Create, "link")
    do (
        base.Console.ClearLogs.OnRequest.AddWatcher linkStats.AsProperty.Owner "LinkStats.ClearLogs" (fun _ ->
            linkStats.ClearLogs ()
        )
    )
    override this.Runner = this
    static member Spawn k m = new Agent<'socket, 'pkt, 'req> (k, m)
    member __.LinkStats : LinkStats = linkStats

let castEvt<'pkt, 'req> : CastEvt<Msg<'pkt, 'req>, Evt<'pkt>> =
    function
    | WebSocketEvt evt -> Some evt
    | _ -> None

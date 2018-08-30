[<AutoOpen>]
module Dap.WebSocket.Types

open System
open System.Net.WebSockets
open System.Threading

open Dap.Prelude
open Dap.Platform

type Encode<'pkt> = 'pkt -> ArraySegment<byte>

// buffer, index, count
type Decode<'pkt> = array<byte> * int * int -> 'pkt

type SendStats = {
    ProcessTime : Instant
    BytesCount : int
    SentTime : Instant
    EncodeDuration : Duration
    TransferDuration : Duration
}

type ReceiveStats = {
    ProcessTime : Instant
    BytesCount : int
    ReceivedTime : Instant
    TransferDuration : Duration
    DecodeDuration : Duration
}

type LinkedStats = {
    ProcessTime : Instant
    ConnectedTime : Instant
    ConnectDuration : Duration
}

type ConnectionStats = {
    Linked : LinkedStats
    ClosedTime : Instant option
    mutable SentCount : int
    mutable ReceivedCount : int
} with
    static member Create linked =
        {
            Linked = linked
            ClosedTime = None
            SentCount = 0
            ReceivedCount = 0
        }

type Agent<'socket, 'pkt, 'req> when 'socket :> WebSocket and 'req :> IReq (param) =
    inherit BaseAgent<Agent<'socket, 'pkt, 'req>, Args<'socket, 'pkt, 'req>, Model<'socket, 'pkt>, Msg<'pkt, 'req>, 'req, Evt<'pkt>> (param)
    override this.Runner = this
    static member Spawn (param) = new Agent<'socket, 'pkt, 'req> (param)

and ActorOperate<'socket, 'pkt, 'req> when 'socket :> WebSocket and 'req :> IReq =
    ActorOperate<Agent<'socket, 'pkt, 'req>, Args<'socket, 'pkt, 'req>, Model<'socket, 'pkt>, Msg<'pkt, 'req>, 'req, Evt<'pkt>>

and Args<'socket, 'pkt, 'req> when 'socket :> WebSocket and 'req :> IReq = {
    LogTraffic : bool
    SendType : WebSocketMessageType
    BufferSize : int
    Encode : Encode<'pkt>
    Decode : Decode<'pkt>
    HandleReq : 'req -> ActorOperate<'socket, 'pkt, 'req>
}

and Evt<'pkt> =
    | OnSent of SendStats * 'pkt
    | OnReceived of ReceiveStats * 'pkt
    | OnStatusChanged of LinkStatus
with interface IEvt

and InternalEvt =
    | OnLinked of LinkedStats
    | TryCloseSocket
    | DoRefreshStatus of exn option

and Msg<'pkt, 'req> =
    | WebSocketReq of 'req
    | WebSocketEvt of Evt<'pkt>
    | InternalEvt of InternalEvt
with interface IMsg

and Model<'socket, 'pkt> when 'socket :> WebSocket = {
    Link : Link<'socket> option
    Stats : ConnectionStats option
    Status : LinkStatus
}

and [<StructuredFormatDisplay("<Link>{AsDisplay}")>]
    Link<'socket> when 'socket :> WebSocket = {
    Ident : string
    Token : CancellationToken
    Socket : 'socket
    Buffer : byte[]
} with
    member this.AsDisplay = (this.Ident, this.Socket)

let castEvt<'pkt, 'req> : CastEvt<Msg<'pkt, 'req>, Evt<'pkt>> =
    function
    | WebSocketEvt evt -> Some evt
    | _ -> None

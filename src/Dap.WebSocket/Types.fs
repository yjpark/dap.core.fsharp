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
    EncodeDuration : Duration
    TransferDuration : Duration
}

type ReceiveStats = {
    ProcessTime : Instant
    BytesCount : int
    TransferDuration : Duration
    DecodeDuration : Duration
}

type Agent<'socket, 'pkt, 'req> when 'socket :> WebSocket and 'req :> IReq =
    IAgent<Args<'socket, 'pkt, 'req>, Model<'socket, 'pkt>, Msg<'pkt, 'req>, 'req, Evt<'pkt>>

and ActorOperate<'socket, 'pkt, 'req> when 'socket :> WebSocket and 'req :> IReq =
    ActorOperate<Args<'socket, 'pkt, 'req>, Model<'socket, 'pkt>, Msg<'pkt, 'req>, 'req, Evt<'pkt>>

and Args<'socket, 'pkt, 'req> when 'socket :> WebSocket and 'req :> IReq = {
    LogTraffic : bool
    SendType : WebSocketMessageType
    BufferSize : int
    Encode : Encode<'pkt>
    Decode : Decode<'pkt>
    HandleReq : 'req -> ActorOperate<'socket, 'pkt, 'req>
}

and Evt<'pkt> =
    | OnConnected
    | OnDisconnected
    | OnSent of SendStats * 'pkt
    | OnReceived of ReceiveStats * 'pkt
with interface IEvt

and Msg<'pkt, 'req> =
    | WebSocketReq of 'req
    | WebSocketEvt of Evt<'pkt>
with interface IMsg

and Model<'socket, 'pkt> when 'socket :> WebSocket = {
    Link : Link<'socket> option
    Connected : bool
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
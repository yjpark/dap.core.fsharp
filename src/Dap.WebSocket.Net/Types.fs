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
    TrasferDuration : Duration
}

type ReceiveStats = {
    ProcessTime : Instant
    BytesCount : int
    TrasferDuration : Duration
    DecodeDuration : Duration
}

type Args<'pkt, 'evt> = {
    LogTraffic : bool
    SendType : WebSocketMessageType
    BufferSize : int
    Encode : Encode<'pkt>
    Decode : Decode<'pkt>
    Event' : Event<'evt>
} with
    member this.FireEvent' = this.Event'.Trigger
    member this.OnEvent = this.Event'.Publish

and IState<'pkt, 'evt, 'socket> when 'socket :> WebSocket = {
    Args : Args<'pkt, 'evt>
    FireEvent : 'evt -> unit
    Ident : string
    Token : CancellationToken
    Socket : 'socket
    Buffer : byte[]
}
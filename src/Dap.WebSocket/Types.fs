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

type ConnectedStats = {
    ProcessTime : Instant
    ConnectDuration : Duration
} with
    static member Create time duration =
        {
            ProcessTime = time
            ConnectDuration = duration
        }

type ConnectionStats = {
    Connected : ConnectedStats
    DisconnectedTime : Instant option
    mutable SentCount : int
    mutable ReceivedCount : int
} with
    static member Create connected =
        {
            Connected = connected
            DisconnectedTime = None
            SentCount = 0
            ReceivedCount = 0
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
    | OnConnected of ConnectedStats
    | OnDisconnected of ConnectionStats option
    | OnSent of SendStats * 'pkt
    | OnReceived of ReceiveStats * 'pkt
with interface IEvt

and Msg<'pkt, 'req> =
    | WebSocketReq of 'req
    | WebSocketEvt of Evt<'pkt>
with interface IMsg

and Model<'socket, 'pkt> when 'socket :> WebSocket = {
    Link : Link<'socket> option
    Stats : ConnectionStats option
    Closing : bool
} with
    member this.Connected = this.Stats.IsSome

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

let fireOnDisconnected (runner : Agent<'socket, 'pkt, 'req>) (e : exn option) =
    runner.Actor.State.Stats
    |> Option.map (fun stats ->
        {stats with DisconnectedTime = Some runner.Clock.Now}
    )|> (fun stats ->
        let link = runner.Actor.State.Link
        match e with
        | None ->
            logInfo runner "Link" "Disconnected" (link, stats)
        | Some e ->
            logException runner "Link" "Disconnected" (link, stats) e
        runner.Deliver <| WebSocketEvt ^<| OnDisconnected stats
    )
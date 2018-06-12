module Dap.WebSocket.Client.Types

open Dap.Prelude
open Dap.Platform

[<Literal>]
let Kind = "WebSocketClient"

type Encode<'pkt> = 'pkt -> obj

// buffer, index, count
type Decode<'pkt> = obj -> 'pkt

type Req<'pkt> =
    | DoConnect
    | DoSend of 'pkt * Callback<System.DateTime>

type Evt<'pkt> =
    | OnConnected
    | OnDisconnected
    | OnSent of 'pkt
    | OnReceived of 'pkt

type Args<'pkt> = {
    Uri : string
    LogTraffic : bool
    Encode : Encode<'pkt>
    Decode : Decode<'pkt>
    Event' : Event<Evt<'pkt>>
} with
    member this.FireEvent' = this.Event'.Trigger
    member this.OnEvent = this.Event'.Publish

type Model<'pkt> = {
    Args : Args<'pkt>
    Socket : Fable.Import.Browser.WebSocket option
    Ready : bool
}

type Msg<'pkt> =
    | WebSocketReq of Req<'pkt>
    | WebSocketEvt of Evt<'pkt>
with interface IMsg

type Actor<'pkt> = IActor<Model<'pkt>, Req<'pkt>, Evt<'pkt>>
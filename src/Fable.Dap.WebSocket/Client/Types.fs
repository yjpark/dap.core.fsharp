module Dap.WebSocket.Client.Types

open Dap.Prelude
open Dap.Platform

[<Literal>]
let Kind = "WebSocketClient"

type Encode<'pkt> = 'pkt -> obj

// buffer, index, count
type Decode<'pkt> = obj -> 'pkt

type Agent<'pkt> = IAgent<Args<'pkt>, Model<'pkt>, Msg<'pkt>, Req<'pkt>, Evt<'pkt>>

and Args<'pkt> = {
    Uri : string
    LogTraffic : bool
    Encode : Encode<'pkt>
    Decode : Decode<'pkt>
}

and Model<'pkt> = {
    Socket : Fable.Import.Browser.WebSocket option
    Connected : bool
}

and Req<'pkt> =
    | DoConnect
    | DoSend of 'pkt * Callback<System.DateTime>
with interface IReq

and Evt<'pkt> =
    | OnConnected
    | OnDisconnected
    | OnSent of 'pkt
    | OnReceived of 'pkt
with interface IEvt

and Msg<'pkt> =
    | WebSocketReq of Req<'pkt>
    | WebSocketEvt of Evt<'pkt>
with interface IMsg

let castEvt<'pkt> : CastEvt<Msg<'pkt>, Evt<'pkt>> =
    function
    | WebSocketEvt evt -> Some evt
    | _ -> None
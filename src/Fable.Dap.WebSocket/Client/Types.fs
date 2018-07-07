module Dap.WebSocket.Client.Types

open Dap.Prelude
open Dap.Platform

type Encode<'pkt> = 'pkt -> obj

// buffer, index, count
type Decode<'pkt> = obj -> 'pkt

type Args<'pkt> = {
    Encode : Encode<'pkt>
    Decode : Decode<'pkt>
    Uri : string
    LogTraffic : bool
} with
    static member Create encode decode uri logTraffic =
        {
            Encode = encode
            Decode = decode
            Uri = uri
            LogTraffic = logTraffic
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

type Agent<'pkt> (param) =
    inherit BaseAgent<Agent<'pkt>, Args<'pkt>, Model<'pkt>, Msg<'pkt>, Req<'pkt>, Evt<'pkt>> (param)
    override this.Runner = this
    static member Spawn (param) = new Agent<'pkt> (param)


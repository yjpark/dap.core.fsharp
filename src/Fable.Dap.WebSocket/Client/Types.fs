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
    AutoConnect : bool
    LogTraffic : bool
} with
    static member Create encode decode uri autoConnect logTraffic =
        {
            Encode = encode
            Decode = decode
            Uri = uri
            AutoConnect = autoConnect
            LogTraffic = logTraffic
        }


and Model<'pkt> = {
    Socket : Fable.Import.Browser.WebSocket option
    Status : LinkStatus
}

and Req<'pkt> =
    | DoConnect
    | DoSend of 'pkt * Callback<System.DateTime>
with interface IReq

and Evt<'pkt> =
    | OnSent of 'pkt
    | OnReceived of 'pkt
    | OnStatusChanged of LinkStatus
with interface IEvt

and InternalEvt =
    | OnLinked
    | OnClosed

and Msg<'pkt> =
    | WebSocketReq of Req<'pkt>
    | WebSocketEvt of Evt<'pkt>
    | InternalEvt of InternalEvt
with interface IMsg

let castEvt<'pkt> : CastEvt<Msg<'pkt>, Evt<'pkt>> =
    function
    | WebSocketEvt evt -> Some evt
    | _ -> None

type Agent<'pkt> (param) =
    inherit BaseAgent<Agent<'pkt>, Args<'pkt>, Model<'pkt>, Msg<'pkt>, Req<'pkt>, Evt<'pkt>> (param)
    override this.Runner = this
    static member Spawn (param) = new Agent<'pkt> (param)


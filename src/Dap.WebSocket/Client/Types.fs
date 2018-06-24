[<AutoOpen>]
module Dap.WebSocket.Client.Types

open System
open System.Net.WebSockets
open System.Threading

open Dap.Prelude
open Dap.Platform
open Dap.WebSocket

type ConnectStats = {
    ProcessTime : Instant
    ConnectDuration : Duration
}

type Agent<'pkt> =  IAgent<Model<'pkt>, Req<'pkt>, Evt<'pkt>>

and Args<'pkt> = Args<'pkt, Evt<'pkt>>

and State<'pkt> = IState<'pkt, Evt<'pkt>, ClientWebSocket>

and Model<'pkt> = {
    Args : Args<'pkt>
    State : State<'pkt> option
}

and Req<'pkt> =
    | DoConnect of string * CancellationToken * Callback<ConnectStats>
    | DoSend of 'pkt * Callback<SendStats>

and Evt<'pkt> =
    | OnConnected of ConnectStats
    | OnDisconnected
    | OnSent of SendStats * 'pkt
    | OnReceived of ReceiveStats * 'pkt

and Msg<'pkt> =
    | WebSocketReq of Req<'pkt>
    | WebSocketEvt of Evt<'pkt>
with interface IMsg

let DoConnect' (uri : string) (token : CancellationToken) callback =
    DoConnect (uri, token, callback)

let DoSend' (pkt : 'pkt) callback =
    DoSend (pkt, callback)
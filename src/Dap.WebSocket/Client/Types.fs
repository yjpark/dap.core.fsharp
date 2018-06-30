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

type Agent<'pkt> =  Dap.WebSocket.Types.Agent<ClientWebSocket, 'pkt, Req<'pkt>>

and Req<'pkt> =
    | DoConnect of string * CancellationToken * Callback<ConnectStats>
    | DoSend of 'pkt * Callback<SendStats>

let DoConnect' (uri : string) (token : CancellationToken) callback =
    DoConnect (uri, token, callback)

let DoSend' (pkt : 'pkt) callback =
    DoSend (pkt, callback)
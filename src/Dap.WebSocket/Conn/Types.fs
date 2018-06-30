[<AutoOpen>]
module Dap.WebSocket.Conn.Types

open System
open System.Threading.Tasks
open System.Net.WebSockets
open System.Threading

open Dap.Prelude
open Dap.Platform
open Dap.WebSocket

type Agent<'pkt> =  Dap.WebSocket.Types.Agent<WebSocket, 'pkt, Req<'pkt>>

and Req<'pkt> =
    | DoConnect of string * CancellationToken * WebSocket * Callback<Task>
    | DoSend of 'pkt * Callback<SendStats>

let DoConnect' (ident : string) (token : CancellationToken) (socket : WebSocket) callback =
    DoConnect (ident, token, socket, callback)

let DoSend' (pkt : 'pkt) callback =
    DoSend (pkt, callback)


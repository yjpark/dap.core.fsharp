[<AutoOpen>]
module Dap.WebSocket.Conn.Types

open System
open System.Threading.Tasks
open System.Net.WebSockets
open System.Threading

open Dap.Prelude
open Dap.Platform
open Dap.WebSocket
open Dap.WebSocket.Types

type Agent<'pkt> =  Dap.WebSocket.Types.Agent<WebSocket, 'pkt, Req<'pkt>>

and Req<'pkt> =
    | DoAttach of string * CancellationToken * WebSocket * Callback<Task>
    | DoSend of 'pkt * Callback<unit>
with interface IReq

let DoAttach (ident : string) (token : CancellationToken) (socket : WebSocket) callback =
    DoAttach (ident, token, socket, callback)

let DoSend (pkt : 'pkt) callback =
    DoSend (pkt, callback)


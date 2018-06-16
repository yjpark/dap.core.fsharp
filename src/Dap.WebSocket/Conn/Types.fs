[<AutoOpen>]
module Dap.WebSocket.Conn.Types

open System
open System.Threading.Tasks
open System.Net.WebSockets
open System.Threading

open Dap.Prelude
open Dap.Platform
open Dap.WebSocket

type Args<'pkt> = Args<'pkt, Evt<'pkt>>

and State<'pkt> = IState<'pkt, Evt<'pkt>, WebSocket>

and Model<'pkt> = {
    Args : Args<'pkt>
    State : State<'pkt> option
}

and Req<'pkt> =
    | DoConnect of string * CancellationToken * WebSocket * Callback<Task>
    | DoSend of 'pkt * Callback<SendStats>

and Evt<'pkt> =
    | OnConnected
    | OnDisconnected
    | OnSent of SendStats * 'pkt
    | OnReceived of ReceiveStats * 'pkt

and Msg<'pkt> =
    | WebSocketReq of Req<'pkt>
    | WebSocketEvt of Evt<'pkt>
with interface IMsg

let DoConnect' (ident : string) (token : CancellationToken) (socket : WebSocket) callback =
    DoConnect (ident, token, socket, callback)

let DoSend' (pkt : 'pkt) callback =
    DoSend (pkt, callback)


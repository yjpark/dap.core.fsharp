[<RequireQualifiedAccess>]
module Dap.WebSocket.Internal.Logic

open System.Net.WebSockets

open Dap.Prelude
open Dap.Platform
open Dap.WebSocket.Types
open Dap.WebSocket.Internal.Tasks

let internal doSend runner onSent (state : IState<'pkt, 'evt, 'socket> option)
                    (msg : IMsg) ((pkt, callback) : 'pkt * Callback<SendStats>) : unit =
    match state with
    | Some state ->
        match state.Socket.State with
        | WebSocketState.Open ->
            replyAsync3 runner msg callback nakOnFailed <| doSendAsync onSent state pkt
        | state ->
            reply runner callback <| nak msg "Invalid_State" state
    | None ->
        reply runner callback <| nak msg "Not_Connected" None

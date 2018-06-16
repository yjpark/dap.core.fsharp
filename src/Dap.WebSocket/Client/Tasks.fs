module Dap.WebSocket.Client.Tasks

open System
open System.Net
open System.Net.WebSockets
open FSharp.Control.Tasks
open Dap.Prelude
open Dap.Platform
open Dap.WebSocket
open Dap.WebSocket.Client.Types

module BaseTasks = Dap.WebSocket.Internal.Tasks

let internal doReceiveFailed (state : State<'pkt>) : OnFailed =
    BaseTasks.doReceiveFailed OnDisconnected state

let internal doReceiveAsync (state : State<'pkt>) : GetTask<unit> =
    BaseTasks.doReceiveAsync OnReceived OnDisconnected state

let internal doSendAsync (state : State<'pkt>) (pkt : 'pkt) : GetReplyTask<SendStats> =
    BaseTasks.doSendAsync OnSent state pkt

let internal doConnectAsync (state : State<'pkt>) : GetReplyTask<ConnectStats> =
    fun msg callback runner -> task {
        let time = runner.Clock.Now
        logInfo runner "Link" "Connecting" state.Ident
        //state.Socket.Options.Proxy <- (new WebProxy("http://127.0.0.1:1104/")) :> IWebProxy
        do! state.Socket.ConnectAsync (Uri(state.Ident), state.Token)
        let (_, duration) = runner.Clock.CalcDuration(time)
        match state.Socket.State with
        | WebSocketState.Open ->
            let stats : ConnectStats = {
                ProcessTime = time
                ConnectDuration = duration
            }
            logInfo runner "Link" "Connected" state.Ident
            reply runner callback <| ack msg stats
            state.FireEvent <| OnConnected stats
        | state' ->
            reply runner callback <| nak msg "Connect_Failed" (time, duration, state')
            state.FireEvent <| OnDisconnected
    }
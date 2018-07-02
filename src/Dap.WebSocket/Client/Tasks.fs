module Dap.WebSocket.Client.Tasks

open System
open System.Net
open System.Net.WebSockets
open FSharp.Control.Tasks
open Dap.Prelude
open Dap.Platform
open Dap.WebSocket
open Dap.WebSocket.Client.Types
open Dap.WebSocket.Internal.Tasks

let internal doConnectAsync : GetReplyTask<Agent<'pkt>, ConnectStats> =
    fun req callback runner -> task {
        let time = runner.Clock.Now
        while Option.isNone runner.Actor.State.Link do
            do! Task.Delay 20
        let link = runner.Actor.State.Link |> Option.get
        logInfo runner "Link" "Connecting" link
        //link.Socket.Options.Proxy <- (new WebProxy("http://127.0.0.1:1104/")) :> IWebProxy
        do! link.Socket.ConnectAsync (Uri(link.Ident), link.Token)
        let (_, duration) = runner.Clock.CalcDuration(time)
        match link.Socket.State with
        | WebSocketState.Open ->
            let stats : ConnectStats = {
                ProcessTime = time
                ConnectDuration = duration
            }
            logInfo runner "Link" "Connected" link
            reply runner callback <| ack req stats
            runner.Deliver <| WebSocketEvt OnConnected
            runner.RunTask3 doReceiveFailed doReceiveAsync
        | state' ->
            reply runner callback <| nak req "Connect_Failed" (link, time, duration, state')
            runner.Deliver <| WebSocketEvt OnDisconnected
    }
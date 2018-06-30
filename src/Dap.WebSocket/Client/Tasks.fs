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
module BaseTypes = Dap.WebSocket.Types

let internal doConnectAsync : GetReplyTask<Agent<'pkt>, ConnectStats> =
    fun msg callback runner -> task {
        let time = runner.Clock.Now
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
            reply runner callback <| ack msg stats
            runner.Actor.Args.FireEvent' <| OnConnected
        | state' ->
            reply runner callback <| nak msg "Connect_Failed" (link, time, duration, state')
            runner.Actor.Args.FireEvent' <| OnDisconnected
    }
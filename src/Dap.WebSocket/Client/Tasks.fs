module Dap.WebSocket.Client.Tasks

open System
open System.Net
open System.Threading.Tasks
open System.Net.WebSockets
open FSharp.Control.Tasks.V2
open Dap.Prelude
open Dap.Platform
open Dap.WebSocket
open Dap.WebSocket.Client.Types
open Dap.WebSocket.Internal.Tasks

let internal doConnectFailed : OnReplyFailed<Agent<'pkt>, ConnectedStats> =
    fun req callback runner e ->
        reply runner callback <| nak req "Connect_Failed" e
        fireOnDisconnected runner None

let internal doConnectAsync : GetReplyTask<Agent<'pkt>, ConnectedStats> =
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
            let stats = ConnectedStats.Create time duration
            logInfo runner "Link" "Connected" link
            reply runner callback <| ack req stats
            runner.Deliver <| WebSocketEvt ^<| OnConnected stats
            runner.AddTask doReceiveFailed doReceiveAsync
        | state' ->
            reply runner callback <| nak req "Connect_Failed" (link, time, duration, state')
            fireOnDisconnected runner None
    }
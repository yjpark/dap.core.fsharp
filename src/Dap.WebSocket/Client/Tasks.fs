module Dap.WebSocket.Client.Tasks

open System
open System.Net
open System.Threading.Tasks
open System.Net.WebSockets
open Dap.Prelude
open Dap.Platform
open Dap.WebSocket
open Dap.WebSocket.Types
open Dap.WebSocket.Client.Types
open Dap.WebSocket.Internal.Tasks

let internal doConnectFailed : OnReplyFailed<Agent<'pkt>, unit> =
    fun req callback runner e ->
        reply runner callback <| nak req "Connect_Failed" e
        runner.Deliver <| InternalEvt ^<| DoRefreshStatus (Some e)

let internal doConnectAsync : GetReplyTask<Agent<'pkt>, unit> =
    fun req callback runner -> task {
        let time = runner.Clock.Now
        let processTime = time
        while Option.isNone runner.Actor.State.Link do
            do! Task.Delay 20
        let link = runner.Actor.State.Link |> Option.get
        logInfo runner "Link" "Connecting" link
        //link.Socket.Options.Proxy <- (new WebProxy("http://127.0.0.1:1104/")) :> IWebProxy
        do! link.Socket.ConnectAsync (Uri(link.Ident), link.Token)
        let (time, duration) = runner.Clock.CalcDuration(time)
        match link.Socket.State with
        | WebSocketState.Open ->
            logInfo runner "Link" "Connected" link
            reply runner callback <| ack req ()
            runner.Deliver <| InternalEvt OnLinked
            runner.AddTask refreshStatusOnFailed doReceiveAsync
        | state ->
            reply runner callback <| nak req "Connect_Failed" (link, time, duration, state)
            runner.Deliver <| InternalEvt ^<| DoRefreshStatus None
    }
module Dap.WebSocket.Internal.Tasks

open System
open System.Threading.Tasks
open System.Net.WebSockets
open FSharp.Control.Tasks
open Dap.Prelude
open Dap.Platform
open Dap.WebSocket.Types

let private doReadPktAsync (link : Link<'socket>)
                            : GetTask<Agent<'socket, 'pkt, 'req>, bool> =
    fun runner -> task {
        let mutable closed = false;
        let mutable offset = 0
        let mutable capacity = link.Buffer.Length
        try
            let time = runner.Clock.Now
            let mutable finished = false
            let socket = link.Socket :> WebSocket
            while not finished do
                //logInfo runner "Dev" "ReceiveAsync" "Begin"
                let! result = socket.ReceiveAsync(ArraySegment<byte>(link.Buffer, offset, capacity), link.Token)
                //logInfo runner "Dev" "ReceiveAsync" "End"
                if result.CloseStatus.HasValue then
                    logInfo runner "Link" "Closed" link
                    finished <- true
                    closed <- true
                else
                    offset <- offset + result.Count
                    capacity <- capacity - result.Count
                    if result.EndOfMessage then
                        let length = offset
                        finished <- true
                        let (time, transferDuration) = runner.Clock.CalcDuration(time)
                        match runner.RunFunc<'pkt> (fun _ -> runner.Actor.Args.Decode (link.Buffer, 0, length)) with
                        | Ok pkt ->
                            let (_time, decodeDuration) = runner.Clock.CalcDuration(time)
                            let stats : ReceiveStats = {
                                ProcessTime = time
                                BytesCount = length
                                TransferDuration = transferDuration
                                DecodeDuration = decodeDuration
                            }
                            if runner.Actor.Args.LogTraffic then
                                logInfo runner "Traffic" "Received" (link, stats, pkt)
                            runner.Actor.Args.FireEvent' <| OnReceived (stats, pkt)
                        | Error e ->
                            logException runner "Received" "Decode_Failed" (link, length) e
        with
        | e ->
            logInfo runner "Received" "Exception_Raised" (link, e)
            closed <- true
        return closed
    }

let internal doReceiveFailed : OnFailed<Agent<'socket, 'pkt, 'req>> =
    fun runner e ->
        logInfo runner "Link" "Disconnected" (runner.Actor.State.Link, e)
        runner.Actor.Args.FireEvent' OnDisconnected

let internal doReceiveAsync : GetTask<Agent<'socket, 'pkt, 'req>, unit> =
    fun runner -> task {
        let link = runner.Actor.State.Link |> Option.get
        let mutable closed = false
        while not closed do
            let! closed' = doReadPktAsync link runner
            closed <- closed'
        let socket = link.Socket :> WebSocket
        if socket.State = WebSocketState.Open then
            logInfo runner "Link" "Closing" link.Ident
            do! socket.CloseAsync (WebSocketCloseStatus.Empty, "", link.Token)
        logInfo runner "Link" "Disconnected" link.Ident
        runner.Actor.Args.FireEvent' OnDisconnected
    }

let internal doSendAsync (pkt : 'pkt) : GetReplyTask<Agent<'socket, 'pkt, 'req>, SendStats> =
    fun msg callback runner -> task {
        let time = runner.Clock.Now
        let buffer = runner.Actor.Args.Encode pkt
        let (time, encodeDuration) = runner.Clock.CalcDuration(time)
        let link = runner.Actor.State.Link |> Option.get
        let socket = link.Socket :> WebSocket
        do! socket.SendAsync (buffer, runner.Actor.Args.SendType, true, link.Token)
        let (_time, transferDuration) = runner.Clock.CalcDuration(time)
        let stats : SendStats = {
            ProcessTime = time
            BytesCount = buffer.Count
            EncodeDuration = encodeDuration
            TransferDuration = transferDuration
        }
        if runner.Actor.Args.LogTraffic then
            logInfo runner "Traffic" "Sent" (stats, pkt)
        reply runner callback <| ack msg stats
        runner.Actor.Args.FireEvent' <| OnSent (stats, pkt)
    }


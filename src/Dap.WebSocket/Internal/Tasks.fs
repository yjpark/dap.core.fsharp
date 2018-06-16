module Dap.WebSocket.Internal.Tasks

open System
open System.Threading.Tasks
open System.Net.WebSockets
open FSharp.Control.Tasks
open Dap.Prelude
open Dap.Platform
open Dap.WebSocket.Types

let private doReadPktAsync (onReceived : ReceiveStats * 'pkt -> 'evt)
                            (state : IState<'pkt, 'evt, 'socket>)
                            : IRunner -> Task<bool> =
    fun runner -> task {
        let mutable closed = false;
        let mutable offset = 0
        let mutable capacity = state.Buffer.Length
        try
            let time = runner.Clock.Now
            let mutable finished = false
            let socket = state.Socket :> WebSocket
            while not finished do
                //logInfo runner "Dev" "ReceiveAsync" "Begin"
                let! result = socket.ReceiveAsync(ArraySegment<byte>(state.Buffer, offset, capacity), state.Token)
                //logInfo runner "Dev" "ReceiveAsync" "End"
                if result.CloseStatus.HasValue then
                    logInfo runner "Link" "Closed" state.Ident
                    finished <- true
                    closed <- true
                else
                    offset <- offset + result.Count
                    capacity <- capacity - result.Count
                    if result.EndOfMessage then
                        let length = offset
                        finished <- true
                        let (time, transferDuration) = runner.Clock.CalcDuration(time)
                        match runner.RunFunc<'pkt> (fun _ -> state.Args.Decode (state.Buffer, 0, length)) with
                        | Ok pkt ->
                            let (_time, decodeDuration) = runner.Clock.CalcDuration(time)
                            let stats : ReceiveStats = {
                                ProcessTime = time
                                BytesCount = length
                                TrasferDuration = transferDuration
                                DecodeDuration = decodeDuration
                            }
                            if state.Args.LogTraffic then
                                logInfo runner "Traffic" "Received" (stats, pkt)
                            state.FireEvent <| onReceived (stats, pkt)
                        | Error e ->
                            logError runner "Received" "Decode_Failed" (length, e)
        with
        | e ->
            logError runner "Received" "Exception_Raised" e
            closed <- true
        return closed
    }

let internal doReceiveFailed (onDisconnected : 'evt) (state : IState<'pkt, 'evt, 'socket>) : OnFailed =
    fun runner e ->
        logInfo runner "Link" "Disconnected" (state.Ident, e)
        state.FireEvent onDisconnected

let internal doReceiveAsync (onReceived : ReceiveStats * 'pkt -> 'evt) (onDisconnected : 'evt)
                            (state : IState<'pkt, 'evt, 'socket>) : GetTask<unit> =
    fun runner -> task {
        let mutable closed = false
        while not closed do
            let! closed' = doReadPktAsync onReceived state runner
            closed <- closed'
        let socket = state.Socket :> WebSocket
        if socket.State = WebSocketState.Open then
            logInfo runner "Link" "Closing" state.Ident
            do! socket.CloseAsync (WebSocketCloseStatus.Empty, "", state.Token)
        logInfo runner "Link" "Disconnected" state.Ident
        state.FireEvent onDisconnected
    }

let internal doSendAsync onSent (state : IState<'pkt, 'evt, 'socket>) (pkt : 'pkt) : GetReplyTask<SendStats> =
    fun msg callback runner -> task {
        let time = runner.Clock.Now
        let buffer = state.Args.Encode pkt
        let (time, encodeDuration) = runner.Clock.CalcDuration(time)
        let socket = state.Socket :> WebSocket
        //logInfo runner "Dev" "SendAsync" "Begin"
        do! socket.SendAsync (buffer, state.Args.SendType, true, state.Token)
        //logInfo runner "Dev" "SendAsync" "End"
        let (_time, transferDuration) = runner.Clock.CalcDuration(time)
        let stats : SendStats = {
            ProcessTime = time
            BytesCount = buffer.Count
            EncodeDuration = encodeDuration
            TrasferDuration = transferDuration
        }
        if state.Args.LogTraffic then
            logInfo runner "Traffic" "Sent" (stats, pkt)
        reply runner callback <| ack msg stats
        state.FireEvent <| onSent (stats, pkt)
    }


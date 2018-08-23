module Dap.WebSocket.Internal.Tasks

open System
open System.Threading.Tasks
open System.Net.WebSockets
open FSharp.Control.Tasks.V2
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
            let processTime = time
            let mutable finished = false
            let socket = link.Socket :> WebSocket
            while not finished do
                if runner.Actor.State.Status = LinkStatus.Closing then
                    finished <- true
                    closed <- true
                else
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
                                let (time, decodeDuration) = runner.Clock.CalcDuration(time)
                                let stats : ReceiveStats = {
                                    ProcessTime = processTime
                                    BytesCount = length
                                    ReceivedTime = time
                                    TransferDuration = transferDuration
                                    DecodeDuration = decodeDuration
                                }
                                if runner.Actor.Args.LogTraffic then
                                    logInfo runner "Traffic" "Received" (link, stats, pkt)
                                runner.Deliver <| WebSocketEvt ^<| OnReceived (stats, pkt)
                            | Error e ->
                                logException runner "Received" "Decode_Failed" (link, length) e
        with
        | e ->
            logException runner "Received" "Exception_Raised" link e
            closed <- true
        return closed
    }

let internal refreshStatusOnFailed : OnFailed<Agent<'socket, 'pkt, 'req>> =
    fun runner e ->
        runner.Deliver <| InternalEvt ^<| DoRefreshStatus (Some e)

let private needCloseSocket (socket : WebSocket) =
    match socket.State with
    | WebSocketState.None ->
        false
    | WebSocketState.Connecting ->
        true
    | WebSocketState.Open ->
        true
    | WebSocketState.CloseSent
    | WebSocketState.CloseReceived ->
        true
    | WebSocketState.Closed
    | WebSocketState.Aborted ->
        false
    | _ ->
        false

let internal doReceiveAsync : GetTask<Agent<'socket, 'pkt, 'req>, unit> =
    fun runner -> task {
        while Option.isNone runner.Actor.State.Link do
            do! Task.Delay 20
        let link = runner.Actor.State.Link |> Option.get
        let mutable closed = false
        while not closed do
            let! closed' = doReadPktAsync link runner
            closed <- closed'
        let socket = link.Socket :> WebSocket
        if needCloseSocket socket then
            logInfo runner "Link" "Closing" link.Ident
            do! socket.CloseAsync (WebSocketCloseStatus.Empty, "", link.Token)
        runner.Deliver <| InternalEvt ^<| DoRefreshStatus None
    }

let internal doSendAsync (pkt : 'pkt) : GetReplyTask<Agent<'socket, 'pkt, 'req>, SendStats> =
    fun req callback runner -> task {
        let time = runner.Clock.Now
        let processTime = time
        let buffer = runner.Actor.Args.Encode pkt
        let (time, encodeDuration) = runner.Clock.CalcDuration(time)
        let link = runner.Actor.State.Link |> Option.get
        let socket = link.Socket :> WebSocket
        try
            do! socket.SendAsync (buffer, runner.Actor.Args.SendType, true, link.Token)
            let (time, transferDuration) = runner.Clock.CalcDuration(time)
            let stats : SendStats = {
                ProcessTime = processTime
                BytesCount = buffer.Count
                SentTime = time
                EncodeDuration = encodeDuration
                TransferDuration = transferDuration
            }
            if runner.Actor.Args.LogTraffic then
                logInfo runner "Traffic" "Sent" (stats, pkt)
            reply runner callback <| ack req stats
            runner.Deliver <| WebSocketEvt ^<| OnSent (stats, pkt)
        with e ->
            logException runner "Send" "Exception_Raised" link e
            if needCloseSocket socket then
                logInfo runner "Link" "Closing" link.Ident
                do! socket.CloseAsync (WebSocketCloseStatus.Empty, "", link.Token)
            runner.Deliver <| InternalEvt ^<| DoRefreshStatus None
    }


module Dap.WebSocket.Internal.Tasks

open System
open System.Threading.Tasks
open System.Net.WebSockets
open Dap.Prelude
open Dap.Platform
open Dap.WebSocket
open Dap.WebSocket.Types
open System.Net.WebSockets

let private doReadPktAsync (link : Link<'socket>)
                            : GetTask<Agent<'socket, 'pkt, 'req>, bool> =
    fun runner -> task {
        let stats = runner.LinkStats.Receive
        let mutable finished = false;
        let mutable offset = 0
        let mutable capacity = link.Buffer.Length
        try
            let time = runner.Clock.Now'
            stats.IncPendingCount ()
            let mutable completed = false
            let socket = link.Socket :> WebSocket
            while not completed do
                if runner.Actor.State.Status = LinkStatus.Closing then
                    completed <- true
                    finished <- true
                else
                    //logInfo runner "Dev" "ReceiveAsync" "Begin"
                    let! result = socket.ReceiveAsync(ArraySegment<byte>(link.Buffer, offset, capacity), link.Token)
                    //logInfo runner "Dev" "ReceiveAsync" "End"
                    if result.CloseStatus.HasValue then
                        logInfo runner "Link" "Closed" link
                        completed <- true
                        finished <- true
                    else
                        offset <- offset + result.Count
                        capacity <- capacity - result.Count
                        if result.EndOfMessage then
                            let length = offset
                            completed <- true
                            let (time, transferDuration) = runner.Clock.CalcDuration(time)
                            match runner.RunFunc<'pkt> (fun _ -> runner.Actor.Args.Decode (link.Buffer, 0, length)) with
                            | Ok pkt ->
                                stats.AddPkt runner length time None |> ignore
                                if runner.Actor.Args.LogTraffic then
                                    logInfo runner "Traffic" "Received" (link, stats, pkt)
                                runner.Deliver <| WebSocketEvt ^<| OnReceived pkt
                            | Error e ->
                                stats.AddPkt runner length time (Some e) |> ignore
                                logException runner "Receive" "Decode_Failed" (link, length) e
        with
        | e ->
            logException runner "Receive" "Exception_Raised" link e
            finished <- true
        return finished
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

let internal tryCloseSocketAsync : GetTask<Agent<'socket, 'pkt, 'req>, unit> =
    fun runner -> task {
        match runner.Actor.State.Link with
        | Some link ->
            let socket = link.Socket :> WebSocket
            if needCloseSocket socket then
                logInfo runner "Link" "Closing" link.Ident
                try
                    do! socket.CloseAsync (WebSocketCloseStatus.Empty, "", link.Token)
                with
                | :? WebSocketException as e ->
                    if e.WebSocketErrorCode = WebSocketError.ConnectionClosedPrematurely then
                        logWarn runner "Link" "ConnectionClosedPrematurely" link.Ident
                    else
                        logException runner "Link" "Exception_Raised" link.Ident e
                | _ as e ->
                    logException runner "Link" "Exception_Raised" link.Ident e
        | None -> ()
        runner.Deliver <| InternalEvt ^<| DoRefreshStatus None
    }

let internal doReceiveAsync : GetTask<Agent<'socket, 'pkt, 'req>, unit> =
    fun runner -> task {
        while Option.isNone runner.Actor.State.Link do
            do! Task.Delay 20
        let link = runner.Actor.State.Link |> Option.get
        let mutable finished = false
        while not finished do
            let! finished' = doReadPktAsync link runner
            finished <- finished'
        do! tryCloseSocketAsync runner
    }

let internal doSendAsync (pkt : 'pkt) : GetReplyTask<Agent<'socket, 'pkt, 'req>, unit> =
    fun req callback runner -> task {
        let stats = runner.LinkStats.Send
        let time = runner.Clock.Now'
        stats.IncPendingCount ()
        let buffer = runner.Actor.Args.Encode pkt
        let link = runner.Actor.State.Link |> Option.get
        let socket = link.Socket :> WebSocket
        try
            do! socket.SendAsync (buffer, runner.Actor.Args.SendType, true, link.Token)
            if runner.Actor.Args.LogTraffic then
                logInfo runner "Traffic" "Sent" (stats, pkt)
            stats.AddPkt runner buffer.Count time None |> ignore
            reply runner callback <| ack req ()
            runner.Deliver <| WebSocketEvt ^<| OnSent pkt
        with e ->
            stats.AddPkt runner buffer.Count time (Some e) |> ignore
            logException runner "Send" "Exception_Raised" link e
            do! tryCloseSocketAsync runner
    }


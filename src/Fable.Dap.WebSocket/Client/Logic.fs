[<RequireQualifiedAccess>]
module Dap.WebSocket.Client.Logic

open Dap.Prelude
open Dap.Platform
open Dap.WebSocket.Client.Types

type ActorOperate<'pkt> = Operate<Agent<'pkt>, Model<'pkt>, Msg<'pkt>>

let private createSocket (runner : Agent<'pkt>)  =
    let socket = Browser.WebSocket.WebSocket.Create runner.Actor.Args.Uri
    socket.onopen <- fun _ ->
        runner.Deliver <| InternalEvt OnLinked
    socket.onclose <- fun _ ->
        runner.Deliver <| InternalEvt OnClosed
    socket.onmessage <- fun m ->
        try
            let pkt = runner.Actor.Args.Decode m.data
            if runner.Actor.Args.LogTraffic then
                logInfo runner "Traffic" "Received" pkt
            runner.Deliver <| WebSocketEvt ^<| OnReceived pkt
        with e ->
            logException runner "Receive" "Decode_Failed" m e
    socket

let internal doSetStatus (status : LinkStatus)  : ActorOperate<'pkt> =
    fun runner (model, cmd) ->
        (runner, model, cmd)
        |-|> updateModel (fun m -> {m with Status = status})
        |=|> addSubCmd WebSocketEvt ^<| OnStatusChanged status

let private doConnect : ActorOperate<'pkt> =
    fun runner (model, cmd) ->
        match model.Socket with
        | None ->
            let socket = createSocket runner
            (runner, model, cmd)
            |-|> updateModel (fun m -> {m with Socket = Some socket})
            |=|> doSetStatus LinkStatus.Linking
        | Some socket ->
            logError runner "Connect" "Socket_Exist" (runner.Actor.Args.Uri, socket)
            (model, cmd)

let private doSend req ((pkt, callback) : 'pkt * Callback<System.DateTime>) : ActorOperate<'pkt> =
    fun runner (model, cmd) ->
        match model.Socket with
        | None ->
            reply runner callback <| nak req "Socket_Not_Exist" runner.Actor.Args.Uri
        | Some socket ->
            if socket.readyState <> Browser.Types.WebSocketState.OPEN then
                reply runner callback <| nak req "Invalid_State" runner.Actor.Args.Uri
            else
                let mutable encoded = false
                try
                    let data = runner.Actor.Args.Encode pkt
                    encoded <- true
                    socket.send data
                    if runner.Actor.Args.LogTraffic then
                        logInfo runner "Traffic" "Sent" pkt
                    reply runner callback <| ack req utcNow
                    runner.Deliver <| WebSocketEvt ^<| OnSent pkt
                with e ->
                    let err = if encoded then "Send_Failed" else "Encode_Failed"
                    logException runner "Send" err pkt e
                    reply runner callback <| nak req err e
        (model, cmd)

let private handleReq (req : Req<'pkt>) : ActorOperate<'pkt> =
    fun runner (model, cmd) ->
        match req with
        | DoConnect ->
            doConnect
        | DoSend (pkt, callback) ->
            doSend req (pkt, callback)
        <| runner <| (model, cmd)

let private handleInternalEvt (evt : InternalEvt) : ActorOperate<'pkt> =
    fun runner (model, cmd) ->
        match evt with
        | OnLinked ->
            doSetStatus LinkStatus.Linked
        | OnClosed ->
            updateModel (fun m -> {m with Socket = None})
            |-|- doSetStatus LinkStatus.Closed
        <| runner <| (model, cmd)

let private update : Update<Agent<'pkt>, Model<'pkt>, Msg<'pkt>> =
    fun runner msg model ->
        match msg with
        | WebSocketReq req -> handleReq req
        | WebSocketEvt evt -> noOperation
        | InternalEvt evt -> handleInternalEvt evt
        <| runner <| (model, noCmd)

let private init : ActorInit<Args<'pkt>, Model<'pkt>, Msg<'pkt>> =
    fun _runner args ->
        let cmd =
            if args.AutoConnect then
                cmdOfMsg <| WebSocketReq DoConnect
            else
                noCmd
        ({
            Socket = None
            Status = LinkStatus.NoLink
        }, cmd)

let spec<'pkt> (args : Args<'pkt>) =
    new ActorSpec<Agent<'pkt>, Args<'pkt>, Model<'pkt>, Msg<'pkt>, Req<'pkt>, Evt<'pkt>>
        (Agent<'pkt>.Spawn, args, WebSocketReq, castEvt<'pkt>, init, update)

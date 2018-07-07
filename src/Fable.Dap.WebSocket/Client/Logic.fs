[<RequireQualifiedAccess>]
module Dap.WebSocket.Client.Logic

open Elmish
open Dap.Prelude
open Dap.Platform
open Dap.WebSocket.Client.Types

type ActorOperate<'pkt> = ActorOperate<Agent<'pkt>, Args<'pkt>, Model<'pkt>, Msg<'pkt>, Req<'pkt>, Evt<'pkt>>

let private createSocket (runner : Agent<'pkt>)  =
    let socket = Fable.Import.Browser.WebSocket.Create runner.Actor.Args.Uri
    socket.onopen <- fun _ ->
        runner.Deliver <| WebSocketEvt OnConnected
    socket.onclose <- fun _ ->
        runner.Deliver <| WebSocketEvt OnDisconnected
    socket.onmessage <- fun m ->
        try
            let pkt = runner.Actor.Args.Decode m.data
            if runner.Actor.Args.LogTraffic then
                logInfo runner "Traffic" "Received" pkt
            runner.Deliver <| WebSocketEvt ^<| OnReceived pkt
        with e ->
            logException runner "Receive" "Decode_Failed" m e
    socket

let private doConnect : ActorOperate<'pkt> =
    fun runner (model, cmd) ->
        match model.Socket with
        | None ->
            let socket = createSocket runner
            ({model with Socket = Some socket}, cmd)
        | Some socket ->
            logError runner "Connect" "Socket_Exist" (runner.Actor.Args.Uri, socket)
            (model, cmd)

let private doSend req ((pkt, callback) : 'pkt * Callback<System.DateTime>) : ActorOperate<'pkt> =
    fun runner (model, cmd) ->
        match model.Socket with
        | None ->
            reply runner callback <| nak req "Socket_Not_Exist" runner.Actor.Args.Uri
            (*
            let res = nak<System.DateTime> req "Socket_Not_Exist" runner.Actor.Args.Uri
            callback
            |> Option.iter (fun c -> c res)
            *)
        | Some socket ->
            if socket.readyState <> socket.OPEN then
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

let private handleEvt (evt : Evt<'pkt>) : ActorOperate<'pkt> =
    fun runner (model, cmd) ->
        match evt with
        | OnConnected ->
            updateModel <| fun m -> {m with Connected = true}
        | OnDisconnected ->
            updateModel <| fun m -> {m with Socket = None ; Connected = false}
        | _ ->
            noOperation
        <| runner <| (model, cmd)

let private update : ActorUpdate<Agent<'pkt>, Args<'pkt>, Model<'pkt>, Msg<'pkt>, Req<'pkt>, Evt<'pkt>> =
    fun runner model msg ->
        match msg with
        | WebSocketReq req ->
            handleReq req
        | WebSocketEvt evt ->
            handleEvt evt
        <| runner <| (model, noCmd)

let private init : ActorInit<Args<'pkt>, Model<'pkt>, Msg<'pkt>> =
    fun _runner _args ->
        ({
            Socket = None
            Connected = false
        }, Cmd.ofMsg <| WebSocketReq DoConnect)

let spec<'pkt> (args : Args<'pkt>) =
    new ActorSpec<Agent<'pkt>, Args<'pkt>, Model<'pkt>, Msg<'pkt>, Req<'pkt>, Evt<'pkt>>
        (Agent<'pkt>.Spawn, args, WebSocketReq, castEvt<'pkt>, init, update)

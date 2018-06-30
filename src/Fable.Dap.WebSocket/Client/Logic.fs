[<RequireQualifiedAccess>]
module Dap.WebSocket.Client.Logic

open Elmish
open Dap.Prelude
open Dap.Platform
open Dap.WebSocket.Client.Types

type ActorOperate<'pkt> = ActorOperate<Args<'pkt>, Model<'pkt>, Msg<'pkt>, Req<'pkt>, Evt<'pkt>>

let private createSocket (runner : Agent<'pkt>)  =
    let socket = Fable.Import.Browser.WebSocket.Create runner.Actor.Args.Uri
    socket.onopen <- fun _ ->
        runner.Actor.Args.FireEvent' <| OnConnected
    socket.onclose <- fun _ ->
        runner.Actor.Args.FireEvent' <| OnDisconnected
    socket.onmessage <- fun m ->
        try
            let pkt = runner.Actor.Args.Decode m.data
            if runner.Actor.Args.LogTraffic then
                logInfo runner "Traffic" "Received" pkt
            runner.Actor.Args.FireEvent' <| OnReceived pkt
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

let private doSend (msg : IMsg) ((pkt, callback) : 'pkt * Callback<System.DateTime>) : ActorOperate<'pkt> =
    fun runner (model, cmd) ->
        match model.Socket with
        | None ->
            let res = nak<IMsg, System.DateTime> msg "Socket_Not_Exist" runner.Actor.Args.Uri
            callback
            |> Option.iter (fun c -> c res)
            //reply<System.DateTime> runner callback res
        | Some socket ->
            if socket.readyState <> socket.OPEN then
                reply runner callback <| nak msg "Invalid_State" runner.Actor.Args.Uri
            else
                let mutable encoded = false
                try
                    let data = runner.Actor.Args.Encode pkt
                    encoded <- true
                    socket.send data
                    if runner.Actor.Args.LogTraffic then
                        logInfo runner "Traffic" "Sent" pkt
                    reply runner callback <| ack msg utcNow
                    runner.Actor.Args.FireEvent' <| OnSent pkt
                with e ->
                    let err = if encoded then "Send_Failed" else "Encode_Failed"
                    logException runner "Send" err pkt e
                    reply runner callback <| nak msg err e
        (model, cmd)

let private handleReq msg (req : Req<'pkt>) : ActorOperate<'pkt> =
    fun runner (model, cmd) ->
        match req with
        | DoConnect ->
            doConnect
        | DoSend (pkt, callback) ->
            doSend msg (pkt, callback)
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

let private update : ActorUpdate<Args<'pkt>, Model<'pkt>, Msg<'pkt>, Req<'pkt>, Evt<'pkt>> =
    fun runner model msg ->
        match msg with
        | WebSocketReq req ->
            handleReq msg req
        | WebSocketEvt evt ->
            handleEvt evt
        <| runner <| (model, Cmd.none)

let private init : ActorInit<Args<'pkt>, Model<'pkt>, Msg<'pkt>, Req<'pkt>, Evt<'pkt>> =
    fun _runner args ->
        ({
            Socket = None
            Connected = false
        }, Cmd.ofMsg <| WebSocketReq DoConnect)

let private subscribe : ActorSubscribe<Args<'pkt>, Model<'pkt>, Msg<'pkt>, Req<'pkt>, Evt<'pkt>> =
    fun runner model ->
        subscribeEvent runner model WebSocketEvt runner.Actor.Args.OnEvent

let logic =
    {
        Init = init
        Update = update
        Subscribe = subscribe
    }

let getSpec (newArgs : NewArgs<Args<'pkt>>) : ActorSpec<Args<'pkt>, Model<'pkt>, Msg<'pkt>, Req<'pkt>, Evt<'pkt>> =
    {
        Logic = logic
        NewArgs = newArgs
        WrapReq = WebSocketReq
        GetOnEvent = fun args -> args.OnEvent
    }

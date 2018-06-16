[<RequireQualifiedAccess>]
module Dap.WebSocket.Client.Logic

open Elmish
open Dap.Prelude
open Dap.Platform
open Dap.WebSocket.Client.Types

let private createSocket (runner : IActor) (model : Model<'pkt>) =
    let socket = Fable.Import.Browser.WebSocket.Create model.Args.Uri
    socket.onopen <- fun _ ->
        model.Args.FireEvent' <| OnConnected
    socket.onclose <- fun _ ->
        model.Args.FireEvent' <| OnDisconnected
    socket.onmessage <- fun m ->
        try
            let pkt = model.Args.Decode m.data
            if model.Args.LogTraffic then
                logInfo runner "Traffic" "Received" pkt
            model.Args.FireEvent' <| OnReceived pkt
        with e ->
            logException runner "Receive" "Decode_Failed" m e
    socket

let private doConnect : Operate<IActor, Model<'pkt>, Msg<'pkt>> =
    fun runner (model, cmd) ->
        match model.Socket with
        | None ->
            let socket = createSocket runner model
            ({model with Socket = Some socket}, cmd)
        | Some socket ->
            logError runner "Connect" "Socket_Exist" (model.Args.Uri, socket)
            (model, cmd)

let private doSend (msg : IMsg) ((pkt, callback) : 'pkt * Callback<System.DateTime>) : Operate<IActor, Model<'pkt>, Msg<'pkt>> =
    fun runner (model, cmd) ->
        match model.Socket with
        | None ->
            let res = nak<IMsg, System.DateTime> msg "Socket_Not_Exist" model.Args.Uri
            callback
            |> Option.iter (fun c -> c res)
            //reply<System.DateTime> runner callback res
        | Some socket ->
            if socket.readyState <> socket.OPEN then
                reply runner callback <| nak msg "Invalid_State" model.Args.Uri
            else
                let mutable encoded = false
                try
                    let data = model.Args.Encode pkt
                    encoded <- true
                    socket.send data
                    if model.Args.LogTraffic then
                        logInfo runner "Traffic" "Sent" pkt
                    reply runner callback <| ack msg utcNow
                    model.Args.FireEvent' <| OnSent pkt
                with e ->
                    let err = if encoded then "Send_Failed" else "Encode_Failed"
                    logException runner "Send" err pkt e
                    reply runner callback <| nak msg err e
        (model, cmd)

let private handleReq msg (req : Req<'pkt>) : Operate<IActor, Model<'pkt>, Msg<'pkt>> =
    fun runner (model, cmd) ->
        match req with
        | DoConnect ->
            doConnect
        | DoSend (pkt, callback) ->
            doSend msg (pkt, callback)
        <| runner <| (model, cmd)

let private handleEvt (evt : Evt<'pkt>) : Operate<IActor, Model<'pkt>, Msg<'pkt>> =
    fun runner (model, cmd) ->
        match evt with 
        | OnConnected -> 
            updateModel <| fun m -> {m with Ready = true}
        | OnDisconnected -> 
            updateModel <| fun m -> {m with Socket = None ; Ready = false}
        | _ ->
            noOperation
        <| runner <| (model, cmd)

let private update : Update<IActor, Model<'pkt>, Msg<'pkt>> =
    fun runner model msg ->
        match msg with
        | WebSocketReq req ->
            handleReq msg req
        | WebSocketEvt evt ->
            handleEvt evt
        <| runner <| (model, Cmd.none)

let private init : Init<IActor, Args<'pkt>, Model<'pkt>, Msg<'pkt>> =
    fun _runner args ->
        ({
            Args = args
            Socket = None
            Ready = false
        }, Cmd.ofMsg <| WebSocketReq DoConnect)

let private subscribe : Subscribe<IActor, Model<'pkt>, Msg<'pkt>> =
    fun runner model ->
        subscribeEvent runner model WebSocketEvt model.Args.OnEvent

let logic : Logic<IActor, Args<'pkt>, Model<'pkt>, Msg<'pkt>> =
    {
        Init = init
        Update = update
        Subscribe = subscribe
    }

let getSpec (newArgs : unit -> Args<'pkt>) : ActorSpec<IActor, Args<'pkt>, Model<'pkt>, Msg<'pkt>, Req<'pkt>, Evt<'pkt>> =
    {
        Logic = logic
        NewArgs = newArgs
        WrapReq = WebSocketReq
        GetOnEvent = fun model -> model.Args.OnEvent
    }

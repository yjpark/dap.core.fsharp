[<RequireQualifiedAccess>]
module Dap.Remote.WebSocketProxy.Logic

open System
open Fable.Core
open Elmish
module E = Thoth.Json.Encode
module D = Thoth.Json.Decode

open Dap.Prelude
open Dap.Platform
open Dap.Remote
open Dap.Remote.Internal
open Dap.Remote.Proxy.Types
open Dap.Remote.WebSocketProxy.Types

module BaseLogic = Dap.Remote.Proxy.Logic

module WebSocketTypes = Dap.WebSocket.Client.Types
module WebSocketAgent = Dap.WebSocket.Client.Agent

let private doReconnect : ActorOperate<'req, 'res, 'evt> =
    fun _runner (model, cmd) ->
        let socket = model.Extra.Socket |> Option.get
        socket.Actor.Handle WebSocketTypes.DoConnect
        (model, cmd)

let internal doSend (runner : Proxy<'req, 'res, 'evt>)
                    (pkt : Packet) (callback : Callback<Instant>) : unit =
    let socket = runner.Actor.State.Extra.Socket |> Option.get
    socket.Actor.Handle <| WebSocketTypes.DoSend (pkt, callback)

[<PassGenericsAttribute>]
let private handlerSocketEvt (evt : WebSocketTypes.Evt<Packet>) : ActorOperate<'req, 'res, 'evt> =
    fun runner (model, cmd) ->
        match evt with
        | WebSocketTypes.OnConnected ->
            BaseLogic.doSendQueue
        | WebSocketTypes.OnDisconnected ->
            addFutureCmd 1.0<second> <| SubEvt DoReconnect
        | WebSocketTypes.OnReceived pkt ->
            BaseLogic.handleClient <| Client.OnReceived pkt
        | _ ->
            noOperation
        <| runner <| (model, noCmd)

[<PassGenericsAttribute>]
let private setSocket (socket : WebSocketTypes.Agent<Packet>) : ActorOperate<'req, 'res, 'evt> =
    fun runner (model, cmd) ->
        match model.Extra.Socket with
        | None ->
            socket.Actor.OnEvent.AddWatcher runner "SocketEvt" (runner.Deliver << SubEvt << SocketEvt)
            updateModel (fun m -> m.WithExtra {m.Extra with Socket = Some socket})
        | Some socket' ->
            logError runner "WebSocketService" "Socket_Exist" (socket', socket)
            noOperation
        <| runner <| (model, cmd)

[<PassGenericsAttribute>]
let internal handleSub (evt : SubEvt) : ActorOperate<'req, 'res, 'evt> =
    match evt with
    | SocketEvt evt -> handlerSocketEvt evt
    | DoReconnect -> doReconnect
    | SetSocket socket -> setSocket socket

let internal doInit : ActorOperate<'req, 'res, 'evt> =
    fun runner (model, cmd) ->
        let encode = fun (pkt : Packet) -> box (pkt.EncodeJson 0)
        let decode = fun (json : obj) ->
            match json with
            | :? string as pkt ->
                decodeJson Packet.JsonDecoder pkt
            | _ ->
                castJson Packet.JsonDecoder json
        let args = WebSocketAgent.Args<Packet>.Create encode decode runner.Actor.Args.Uri false
        let socket = runner.Env |> WebSocketAgent.spawn runner.Ident.Key args :?> WebSocketTypes.Agent<Packet>
        (runner, model, cmd)
        |=|> addSubCmd SubEvt ^<| SetSocket socket

let internal calcConnected : Extra -> bool =
    fun extra ->
        extra.Socket
        |> Option.map (fun s -> s.Actor.State.Connected)
        |> Option.defaultValue false
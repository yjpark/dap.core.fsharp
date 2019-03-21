[<RequireQualifiedAccess>]
module Dap.Remote.WebSocketProxy.Logic

open System
open Fable.Core

open Dap.Prelude
open Dap.Context
open Dap.Platform
open Dap.Remote
open Dap.Remote.Internal
open Dap.Remote.Proxy.Types
open Dap.Remote.WebSocketProxy.Types

module BaseLogic = Dap.Remote.Proxy.Logic

module WebSocketTypes = Dap.WebSocket.Client.Types
module WebSocketLogic = Dap.WebSocket.Client.Logic

let private doReconnect : ActorOperate<'req, 'res, 'evt> =
    fun _runner (model, cmd) ->
        let socket = model.Extra.Socket |> Option.get
        socket.Actor.Handle WebSocketTypes.DoConnect
        (model, cmd)

let internal doSend (runner : Proxy<'req, 'res, 'evt>)
                    (pkt : Packet) (callback : Callback<DateTime>) : unit =
    let socket = runner.Actor.State.Extra.Socket |> Option.get
    socket.Actor.Handle <| WebSocketTypes.DoSend (pkt, callback)

let private handlerSocketEvt (evt : WebSocketTypes.Evt<Packet>) : ActorOperate<'req, 'res, 'evt> =
    fun runner (model, cmd) ->
        match evt with
        | WebSocketTypes.OnReceived pkt ->
            BaseLogic.handleClient <| Client.OnReceived pkt
        | WebSocketTypes.OnStatusChanged status ->
            addSubCmd InternalEvt <| DoSetStatus status
            |-|- (
                runner.Actor.Args.RetryDelay
                |> Option.map (fun retryDelay ->
                    match status with
                    | LinkStatus.Closed ->
                        addFutureCmd retryDelay <| SubEvt DoReconnect
                    | _ ->
                        noOperation
                )|> Option.defaultValue noOperation
            )
        | _ ->
            noOperation
        <| runner <| (model, noCmd)

let private setSocket (socket : WebSocketTypes.Agent<Packet>) : ActorOperate<'req, 'res, 'evt> =
    fun runner (model, cmd) ->
        match model.Extra.Socket with
        | None ->
            socket.Actor.OnEvent.AddWatcher runner "SocketEvt" (runner.Deliver << SubEvt << SocketEvt)
            updateModel (fun m -> m.WithExtra {m.Extra with Socket = Some socket})
        | Some socket' ->
            logError runner "WebSocketGateway" "Socket_Exist" (socket', socket)
            noOperation
        <| runner <| (model, cmd)

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
        let args = WebSocketTypes.Args<Packet>.Create encode decode runner.Actor.Args.Uri runner.Actor.Args.AutoConnect false
        let spec = WebSocketLogic.spec args
        let socket =
            runner.Env
            |> Env.spawn spec PacketClientKind runner.Ident.Key
        (runner, model, cmd)
        |=|> addSubCmd SubEvt ^<| SetSocket socket

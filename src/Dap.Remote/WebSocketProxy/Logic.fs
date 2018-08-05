[<RequireQualifiedAccess>]
module Dap.Remote.WebSocketProxy.Logic

open System
open System.Threading

open Dap.Prelude
open Dap.Platform
open Dap.Remote
open Dap.Remote.Internal
open Dap.Remote.Proxy.Types
open Dap.Remote.WebSocketProxy.Types
open Dap.Remote.WebSocketProxy.Tasks

module BaseLogic = Dap.Remote.Proxy.Logic

module WebSocketTypes = Dap.WebSocket.Types
module WebSocketClientTypes = Dap.WebSocket.Client.Types

let private doReconnect : ActorOperate<'req, 'res, 'evt> =
    fun runner (model, cmd) ->
        let socket = model.Extra.Socket |> Option.get
        let cts = new CancellationTokenSource ()
        socket.Actor.Handle <| WebSocketClientTypes.DoConnect runner.Actor.Args.Uri cts.Token None
        (runner, model, cmd)
        |=|> updateExtra (fun x -> {x with Cts = cts})

let internal doSend (runner : Proxy<'req, 'res, 'evt>)
                    (pkt : Packet) (callback : Callback<DateTime>) : unit =
    let callback =
        callback
        |> Callback.wrap (fun (stats : WebSocketTypes.SendStats) ->
            stats.SentTime |> toDateTimeUtc
        )
    let socket = runner.Actor.State.Extra.Socket |> Option.get
    socket.Actor.Handle <| WebSocketClientTypes.DoSend pkt callback

let private handlerSocketEvt (evt : WebSocketTypes.Evt<Packet>) : ActorOperate<'req, 'res, 'evt> =
    fun runner (model, cmd) ->
        match evt with
        | WebSocketTypes.OnConnected _stats ->
            BaseLogic.doSendQueue
        | WebSocketTypes.OnDisconnected _stats ->
            addFutureCmd 1.0<second> <| SubEvt DoReconnect
        | WebSocketTypes.OnReceived (_stats, pkt) ->
            BaseLogic.handleClient <| Client.OnReceived pkt
        | _ ->
            noOperation
        <| runner <| (model, noCmd)

let private setSocket (socket : WebSocketClientTypes.Agent<Packet>) : ActorOperate<'req, 'res, 'evt> =
    fun runner (model, cmd) ->
        match model.Extra.Socket with
        | None ->
            socket.Actor.OnEvent.AddWatcher runner "SocketEvt" (runner.Deliver << SubEvt << SocketEvt)
            updateModel (fun m -> m.WithExtra {m.Extra with Socket = Some socket})
            |-|- addFutureCmd 1.0<second> ^<| SubEvt DoReconnect
        | Some socket' ->
            logError runner "WebSocketService" "Socket_Exist" (socket', socket)
            noOperation
        <| runner <| (model, cmd)

let internal handleSub (evt : SubEvt) : ActorOperate<'req, 'res, 'evt> =
    match evt with
    | SocketEvt evt -> handlerSocketEvt evt
    | DoReconnect -> doReconnect
    | SetSocket socket -> setSocket socket

let internal doInit : ActorOperate<'req, 'res, 'evt> =
    fun runner (model, cmd) ->
        runner.AddTask ignoreOnFailed setSocketAsync
        (model, cmd)

let internal calcConnected : Extra -> bool =
    fun extra ->
        extra.Socket
        |> Option.map (fun s -> s.Actor.State.Connected)
        |> Option.defaultValue false
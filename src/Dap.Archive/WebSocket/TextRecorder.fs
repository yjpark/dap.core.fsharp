[<RequireQualifiedAccess>]
module Dap.Archive.WebSocket.TextRecorder

open FSharp.Control.Tasks.V2
open Dap.Prelude
open Dap.Platform
open Dap.Remote
open Dap.Archive

module RecorderTypes = Dap.Archive.Recorder.Types
module EventRecorder = Dap.Archive.Recorder.EventRecorder

module WebSocketTypes = Dap.WebSocket.Types
module TextClient = Dap.WebSocket.Client.TextClient
module TextConn = Dap.WebSocket.Conn.TextConn

let watch (agent : EventRecorder.Agent) (onEvent : IBus<WebSocketTypes.Evt<string>>) =
    onEvent.AddWatcher agent "TextRecorder.watchClient" (fun evt ->
        match evt with
        | WebSocketTypes.OnSent (_stat, pkt) ->
            EventRecorder.appendEvent' agent "OnSent" pkt
        | WebSocketTypes.OnReceived (_stat, pkt) ->
            EventRecorder.appendEvent' agent "OnReceived" pkt
        | WebSocketTypes.OnConnected stats ->
            EventRecorder.appendEvent' agent "OnConnected" <| sprintf "%A" stats
        | WebSocketTypes.OnDisconnected stats ->
            EventRecorder.appendEvent' agent "OnDisconnected" <| sprintf "%A" stats
    )

let createForClientAsync (profile : Profile) (param : EventRecorder.BundleParam') (client : TextClient.Agent) = task {
    let recorderKey = sprintf "%s_%s" client.Ident.Kind client.Ident.Key
    let! (recorder, _) = client.Env.HandleAsync <| DoGetAgent' EventRecorder.Kind recorderKey
    let recorder = recorder :?> EventRecorder.Agent
    let! meta = recorder.PostAsync <| RecorderTypes.DoBeginRecording' (EventRecorder.createBundle' profile param)
    logInfo recorder "Recorder" "Start_Recording" (recorder.Ident, meta)
    watch recorder client.Actor.OnEvent
    return recorder
}
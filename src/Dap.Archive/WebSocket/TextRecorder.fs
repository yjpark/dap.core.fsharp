[<RequireQualifiedAccess>]
module Dap.Archive.WebSocket.TextRecorder

open Dap.Prelude
open Dap.Context
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
        | WebSocketTypes.OnSent pkt ->
            EventRecorder.appendEvent' agent "OnSent" pkt
        | WebSocketTypes.OnReceived pkt ->
            EventRecorder.appendEvent' agent "OnReceived" pkt
        | WebSocketTypes.OnStatusChanged status ->
            EventRecorder.appendEvent' agent "OnStatusChanged" <| sprintf "%A" status
    )

let createForClientAsync (profile : Profile) (param : EventRecorder.BundleParam') (client : IAgent<TextClient.Req, TextClient.Evt>) = task {
    let recorderKey = sprintf "%s_%s" client.Ident.Kind client.Ident.Key
    let! (recorder, _) = client.Env.HandleAsync <| DoGetAgent EventRecorder.Kind recorderKey
    let recorder = recorder :?> EventRecorder.Agent
    let! meta = recorder.PostAsync <| RecorderTypes.DoBeginRecording (EventRecorder.createBundle' profile param)
    logInfo recorder "Recorder" "Start_Recording" (recorder.Ident, meta)
    watch recorder client.Actor2.OnEvent
    return recorder
}
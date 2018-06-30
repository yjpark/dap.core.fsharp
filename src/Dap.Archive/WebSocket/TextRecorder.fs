[<RequireQualifiedAccess>]
module Dap.Archive.WebSocket.TextRecorder

open FSharp.Control.Tasks
open Dap.Prelude
open Dap.Platform
open Dap.Remote
open Dap.Archive

module RecorderTypes = Dap.Archive.Recorder.Types
module EventRecorder = Dap.Archive.Recorder.EventRecorder

module WebSocketTypes = Dap.WebSocket.Types
module TextClient = Dap.WebSocket.Client.TextClient
module TextConn = Dap.WebSocket.Conn.TextConn

let watchClient (agent : EventRecorder.Agent) (onEvent : IBus<WebSocketTypes.Evt<string>>) =
    onEvent.AddWatcher agent "TextRecorder.watchClient" (fun evt ->
        match evt with
        | WebSocketTypes.OnSent (_stat, pkt) ->
            EventRecorder.appendEvent' agent "OnSent" pkt
        | WebSocketTypes.OnReceived (_stat, pkt) ->
            EventRecorder.appendEvent' agent "OnReceived" pkt
        | _ -> ()
    )

let watchConn (agent : EventRecorder.Agent) (onEvent : IBus<WebSocketTypes.Evt<string>>) =
    onEvent.AddWatcher agent "TextRecorder.watchClient" (fun evt ->
        match evt with
        | WebSocketTypes.OnSent (_stat, pkt) ->
            EventRecorder.appendEvent' agent "OnSent" pkt
        | WebSocketTypes.OnReceived (_stat, pkt) ->
            EventRecorder.appendEvent' agent "OnReceived" pkt
        | _ -> ()
    )

let createForClientAsync (agent : IAgent) (profile : Profile) (param : EventRecorder.BundleParam') (client : TextClient.Agent) = task {
    let! (recorder, _) = agent.Env.HandleAsync <| DoGetAgent' EventRecorder.Kind client.Ident.Key
    let recorder = recorder :?> EventRecorder.Agent
    let! meta = recorder.PostAsync <| RecorderTypes.DoBeginRecording' (EventRecorder.createBundle' profile param)
    logInfo agent "Recorder" "Start_Recording" (recorder.Ident, meta)
    watchClient recorder client.Actor.OnEvent
    return recorder
}
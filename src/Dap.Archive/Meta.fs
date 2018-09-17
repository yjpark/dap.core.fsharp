module Dap.Archive.Meta

open Dap.Prelude
open Dap.Context
open Dap.Context.Meta
open Dap.Platform.Meta

type M with
    static member eventRecorderSpawner (kind : Kind) =
        let alias = "EventRecorder", "Dap.Archive.Recorder.EventRecorder"
        let args = JsonArgs "EventRecorder.Args"
        let type' = "IAgent<EventRecorder.Req, EventRecorder.Evt>"
        let spec = "Dap.Archive.Recorder.Logic.spec"
        M.spawner ([alias], args, type', spec, kind)
    static member eventRecorderSpawner () =
        M.eventRecorderSpawner (Dap.Archive.Recorder.EventRecorder.Kind)

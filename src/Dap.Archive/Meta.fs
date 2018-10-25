module Dap.Archive.Meta

open Dap.Prelude
open Dap.Context
open Dap.Context.Meta
open Dap.Platform.Meta

type M with
    static member eventRecorderSpawner (?kind : Kind) =
        let kind = defaultArg kind Dap.Archive.Recorder.EventRecorder.Kind
        let alias = "EventRecorder", "Dap.Archive.Recorder.EventRecorder"
        let args = JsonArgs "EventRecorder.Args"
        let type' = "EventRecorder.Agent"
        let spec = "EventRecorder.spec"
        M.agent (args, type', spec, kind, aliases = [alias])

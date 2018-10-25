[<AutoOpen>]
module Dap.Platform.Console

open Dap.Prelude
open Dap.Context

[<AbstractClass>]
type Console<'console when 'console :> IConsole> (kind : Kind, logging : ILogging, clock : IClock) =
    inherit BaseConsole<'console> (kind, logging)
    do (
        let stats = base.Properties
        stats.Task.SlowCap.SetValue DefaultTaskSlowCap
        base.GetStats.SetupHandler (fun () ->
            stats.Time.SetValue (clock.Now')
            toJson stats
        )
        base.ClearStats.SetupHandler (fun () ->
            stats.Deliver.ClearStats ()
            stats.Process.ClearStats ()
            stats.Reply.ClearStats ()
            stats.Func.ClearStats ()
            stats.Task.ClearStats ()
        )
    )
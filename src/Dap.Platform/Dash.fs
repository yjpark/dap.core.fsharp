[<AutoOpen>]
module Dap.Platform.Dash'

open Dap.Prelude
open Dap.Context

type IDash with
    member this.Stats = this.Properties.Stats

[<AbstractClass>]
type Dash<'dash when 'dash :> IDash> (kind : Kind, logging : ILogging, clock : IClock) =
    inherit BaseDash<'dash> (kind, logging)
    do (
        let props = base.Properties
        let stats = base.Stats
        stats.Task.SlowCap.SetValue DefaultTaskSlowCap
        base.Inspect.SetupHandler (fun () ->
            props.Time.SetValue (clock.Now')
            toJson props
        )
        base.ClearStats.SetupHandler (fun () ->
            stats.Deliver.ClearStats ()
            stats.Process.ClearStats ()
            stats.Reply.ClearStats ()
            stats.Func.ClearStats ()
            stats.Task.ClearStats ()
        )
    )
[<AutoOpen>]
module Dap.Platform.Console

open Dap.Prelude
open Dap.Context

type IConsole =
    inherit IContext
    abstract Stats : Stats with get
    abstract GetStats : IHandler<unit, Json> with get
    abstract ClearStats : IHandler<unit, unit> with get

[<AbstractClass>]
type Console<'console when 'console :> IConsole> (kind : Kind, logging : ILogging, clock : IClock) as this =
    inherit CustomContext<'console, ContextSpec<Stats>, Stats> (logging, new ContextSpec<Stats>(kind, Stats.Create))
    let getStats = this.Handlers.AddUnitJson ("get_stats")
    let clearStats = this.Handlers.AddUnitUnit ("clear_logs")
    do (
        let stats = this.Properties
        stats.Task.SlowCap.SetValue DefaultTaskSlowCap
        getStats.SetHandler (fun () ->
            stats.Time.SetValue (clock.Now')
            toJson this.Properties
        )
        clearStats.SetHandler (fun () ->
            stats.Deliver.ClearStats ()
            stats.Process.ClearStats ()
            stats.Reply.ClearStats ()
            stats.Func.ClearStats ()
            stats.Task.ClearStats ()
        )
    )
    member __.Stats = this.Properties
    member __.GetStats = getStats
    member __.ClearStats = clearStats
    interface IConsole with
        member __.Stats = this.Stats
        member __.GetStats = this.GetStats
        member __.ClearStats = this.ClearStats
[<AutoOpen>]
module Dap.Platform.Console

open Dap.Prelude
open Dap.Context

type IConsole =
    inherit IContext
    abstract Stats : Stats with get
    abstract GetStats : IHandler<unit, Json> with get
    abstract ClearLogs : IHandler<unit, unit> with get

[<AbstractClass>]
type Console<'console when 'console :> IConsole> (kind : Kind, logging : ILogging, clock : IClock) as this =
    inherit CustomContext<'console, ContextSpec<Stats>, Stats> (logging, new ContextSpec<Stats>(kind, Stats.Create))
    let getStats = this.Handlers.AddUnitJson ("get_stats")
    let clearLogs = this.Handlers.AddUnitUnit ("clear_logs")
    do (
        let stats = this.Properties
        stats.Task.SlowCap.SetValue DefaultTaskSlowCap
        getStats.SetHandler (fun () ->
            stats.Time.SetValue (clock.Now')
            E.json this.Properties
        )
        clearLogs.SetHandler (fun () ->
            stats.Deliver.ClearLogs ()
            stats.Process.ClearLogs ()
            stats.Reply.ClearLogs ()
            stats.Func.ClearLogs ()
            stats.Task.ClearLogs ()
        )
    )
    member __.Stats = this.Properties
    member __.GetStats = getStats
    member __.ClearLogs = clearLogs
    interface IConsole with
        member __.Stats = this.Stats
        member __.GetStats = this.GetStats
        member __.ClearLogs = this.ClearLogs
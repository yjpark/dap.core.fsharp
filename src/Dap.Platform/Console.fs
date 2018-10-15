[<AutoOpen>]
module Dap.Platform.Console

open Dap.Prelude
open Dap.Context

type IConsole =
    inherit IContext
    abstract Stats : Stats with get
    abstract GetStats : IHandler<unit, Json> with get

[<AbstractClass>]
type Console<'console when 'console :> IConsole> (kind : Kind, logging : ILogging, clock : IClock) as this =
    inherit CustomContext<'console, ContextSpec<Stats>, Stats> (logging, new ContextSpec<Stats>(kind, Stats.Create))
    let getStats = this.Handlers.AddUnitJson ("get_stats")
    do (
        getStats.SetHandler (fun () ->
            this.Properties.Time.SetValue (clock.Now')
            E.json this.Properties
        )
    )
    member __.Stats = this.Properties
    member __.GetStats = getStats
    interface IConsole with
        member __.Stats = this.Stats
        member __.GetStats = this.GetStats

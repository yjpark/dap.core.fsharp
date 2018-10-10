[<AutoOpen>]
module Dap.Platform.Console

open Dap.Prelude
open Dap.Context

type IConsole =
    inherit IContext
    abstract Stats : Stats with get

[<AbstractClass>]
type Console<'console when 'console :> IConsole> (kind : Kind, logging : ILogging) =
    inherit CustomContext<'console, ContextSpec<Stats>, Stats> (logging, new ContextSpec<Stats>(kind, Stats.Create))
    member this.Stats = this.Properties
    interface IConsole with
        member this.Stats = this.Stats
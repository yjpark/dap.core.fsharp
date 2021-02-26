module Demo.Helper

open Dap.Prelude
open Dap.Context

[<Literal>]
let Scope = "Demo"

type App with
    static member Create (consoleLogLevel : LogLevel) =
        let logging = setupConsole consoleLogLevel
        let args =
            AppArgs.Create  ()
            |> AppArgs.SetScope Scope
        new App (logging, args)
    static member Create () =
        App.Create (LogLevelInformation)

let app = App.Create ()
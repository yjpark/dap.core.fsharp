module Demo.Helper

open Dap.Prelude
open Dap.Context

[<Literal>]
let Scope = "Demo"

type App with
    static member Create (consoleLogLevel : LogLevel) =
        let logging = setupConsole consoleLogLevel
        let app = new App (logging, Scope)
        app.Setup AppArgs.Default
    static member Create () =
        App.Create (LogLevelInformation)

let app = App.Create ()
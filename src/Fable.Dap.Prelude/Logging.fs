[<AutoOpen>]
module Dap.Prelude.Logging'

open Dap.Prelude

type ConsoleLogger (minimumLevel : int, prefix : string) =
    member _this.MinimumLevel = minimumLevel
    member _this.Prefix = prefix
    member this.Log (evt : LogEvent) =
        if this.MinimumLevel <= evt.Level.ToInt then
            let message = sprintf "[%s] %s%s" evt.Level.ToShortString this.Prefix evt.Format
            let args = List.toArray evt.Params
            match evt.Level with
            | LogLevelFatal | LogLevelError -> 
                Fable.Import.Browser.console.error (message, args)
            | LogLevelWarning ->
                Fable.Import.Browser.console.warn (message, args)
            | LogLevelInformation ->
                Fable.Import.Browser.console.info (message, args)
            | LogLevelDebug | LogLevelVerbose ->
                Fable.Import.Browser.console.trace (message, args)
            match evt.Exception with
            | None -> ()
            | Some e ->
                Fable.Import.Browser.console.error ("Exception:", [|box e.Message ; box "\nStackTrace:" ; box e.StackTrace|])
        else
            ()
    with
        interface ILogger with
            member this.Log evt = this.Log evt

type ConsoleLogging (minimumLevel : int) =
    member this.Logger = ConsoleLogger (minimumLevel, "")
    with
        interface ILogging with
            member this.Close () = ()
            member this.GetLogger (context : string) : ILogger =
                ConsoleLogger (this.Logger.MinimumLevel, sprintf "<%s> " context)
                :> ILogger
        interface ILogger with
            member this.Log evt = this.Logger.Log evt

let setupConsole (minimumLevel : LogLevel option) =
    let minimumLevel = (defaultArg minimumLevel LogLevelInformation).ToInt
    ConsoleLogging (minimumLevel)
    |> setLogging'

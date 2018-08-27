[<AutoOpen>]
module Dap.Prelude.Logging'

open Dap.Prelude

type ConsoleLoggerEnricher = {
    Prefix : (string -> string) option
} with
    static member Nothing =
        {
            Prefix = None
        }
    static member ForPrefix enrich =
        {
            Prefix = Some enrich
        }
    member this.EnrichPrefix (prefix : string) =
        this.Prefix
        |> Option.map (fun enrich ->
            enrich prefix
        )|> Option.defaultValue prefix

type ConsoleLogger (minimumLevel : int, prefix : string,
                    enricher : ConsoleLoggerEnricher) =
    member __.MinimumLevel = minimumLevel
    member __.Prefix = prefix
    member __.Enricher = enricher
    member this.Log (evt : LogEvent) =
        if this.MinimumLevel <= evt.Level.ToInt then
            let time = System.DateTime.UtcNow.ToString("HH:mm:ss.fff")
            let prefix = this.Enricher.EnrichPrefix this.Prefix
            let message = sprintf "[%s] %s%s" evt.Level.ToShortString prefix evt.Format
            let args = List.toArray evt.Params
            match evt.Level with
            | LogLevelFatal | LogLevelError ->
                Fable.Import.Browser.console.error (time, message, args)
            | LogLevelWarning ->
                Fable.Import.Browser.console.warn (time, message, args)
            | LogLevelInformation ->
                Fable.Import.Browser.console.info (time, message, args)
            | LogLevelDebug | LogLevelVerbose ->
                Fable.Import.Browser.console.trace (time, message, args)
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
    member this.Logger = ConsoleLogger (minimumLevel, "", ConsoleLoggerEnricher.Nothing)
    with
        interface ILogging with
            member this.Close () = ()
            member this.GetLogger (context : string) : ILogger =
                ConsoleLogger (this.Logger.MinimumLevel, sprintf "<%s> " context, ConsoleLoggerEnricher.Nothing)
                :> ILogger
        interface ILogger with
            member this.Log evt = this.Logger.Log evt

let setupConsole (minimumLevel : LogLevel) =
    let minimumLevel = minimumLevel.ToInt
    ConsoleLogging (minimumLevel)
    |> setLogging'

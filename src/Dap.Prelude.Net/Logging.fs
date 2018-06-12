[<AutoOpen>]
module Dap.Prelude.Logging'

open System.IO
open Serilog

type ILogger = Dap.Prelude.Logging.ILogger

type LogLevel with
    member this.ToSerilogLevel =
        match this with
        | LogLevelFatal -> Serilog.Events.LogEventLevel.Fatal
        | LogLevelError -> Serilog.Events.LogEventLevel.Error
        | LogLevelWarning -> Serilog.Events.LogEventLevel.Warning
        | LogLevelInformation -> Serilog.Events.LogEventLevel.Information
        | LogLevelDebug -> Serilog.Events.LogEventLevel.Debug
        | LogLevelVerbose -> Serilog.Events.LogEventLevel.Verbose

let private toSerilog (logger : Serilog.ILogger) (evt : LogEvent) =
    match evt.Exception with
    | None ->
        match evt.Level with
        | LogLevelFatal | LogLevelError ->
            let stackTrace = (System.Diagnostics.StackTrace(2)).ToString()
            let format = evt.Format + "\n{StackTrace}"
            let params' = evt.Params @ [stackTrace]
            logger.Write(evt.Level.ToSerilogLevel, format, List.toArray params')
        | _ ->
            logger.Write(evt.Level.ToSerilogLevel, evt.Format, List.toArray evt.Params)
    | Some e ->
        logger.Write(evt.Level.ToSerilogLevel, e, evt.Format, List.toArray evt.Params)

type private ProxyLogger = { 
    Target : Serilog.ILogger
} with
    interface ILogger with
        member this.Log (evt : LogEvent) = 
            toSerilog this.Target evt

type SerilogLogging = {
    Logger : Serilog.ILogger
} with
    member _this.Close () : unit =
        Serilog.Log.CloseAndFlush()
    interface ILogging with
        member this.Close () = this.Close ()
        member this.GetLogger (context : string) : ILogger =
            {
                Target = this.Logger.ForContext("Context", context)
            }
            :> ILogger
    interface ILogger with
        member this.Log (evt : LogEvent) =
            toSerilog this.Logger evt

type AddSink = Serilog.LoggerConfiguration -> Serilog.LoggerConfiguration

let setupSerilog (sinks : AddSink list) : SerilogLogging =
    let config = Serilog.LoggerConfiguration ()
    let config = config.Enrich.WithThreadId ()
    let config = config.Enrich.WithDemystifiedStackTraces ()
    let config = sinks |> List.fold (fun c addSink -> addSink c) config
    Serilog.Log.Logger <- config.CreateLogger()
    {
        Logger = Serilog.Log.Logger
    }
    |> setLogging'

let addConsoleSink (minimumLevel : LogLevel option) : AddSink =
    fun config ->
        let minimumLevel = defaultArg minimumLevel LogLevelInformation
        let theme = Serilog.Sinks.SystemConsole.Themes.AnsiConsoleTheme.Code
        Serilog.ConsoleLoggerConfigurationExtensions.Console(config.WriteTo,
            restrictedToMinimumLevel = minimumLevel.ToSerilogLevel,
            outputTemplate = "{Timestamp:HH:mm:ss.fff} {Level:u3} <{Context}> {Message:lj}{NewLine}{Exception}",
            theme = theme)

let private checkDirectory (path : string) =
    let dirInfo = (new FileInfo (path)).Directory;
    if not dirInfo.Exists then
        dirInfo.Create();
let addFileSink (path : string) : AddSink =
    checkDirectory path
    fun config ->
        Serilog.FileLoggerConfigurationExtensions.File(config.WriteTo,
            Serilog.Formatting.Compact.CompactJsonFormatter(),
            path)

let addRollingFileSink (rollingInterval : RollingInterval) (path : string) : AddSink =
    checkDirectory path
    fun config ->
        Serilog.FileLoggerConfigurationExtensions.File(config.WriteTo,
            Serilog.Formatting.Compact.CompactJsonFormatter(),
            path, rollingInterval = rollingInterval)

let addDailyFileSink : string -> AddSink = 
    addRollingFileSink RollingInterval.Day

let addHourlyFileSink : string -> AddSink = 
    addRollingFileSink RollingInterval.Hour

let addSeqSink (uri : string) : AddSink =
    fun config ->
        Serilog.SeqLoggerConfigurationExtensions.Seq(config.WriteTo, uri)

let setupConsole (minimumLevel : LogLevel option) =
    setupSerilog [ addConsoleSink minimumLevel ]
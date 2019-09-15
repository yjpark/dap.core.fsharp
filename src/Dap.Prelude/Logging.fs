[<AutoOpen>]
module Dap.Prelude.Logging'

open System
open System.IO
open Serilog

[<Literal>]
let DAP_PRELUDE_CONSOLE_USE_JSON_FORMATTER = "DAP_PRELUDE_USE_JSON_FORMATTER";

[<Literal>]
let DAP_PRELUDE_FILE_USE_TEXT_FORMATTER = "DAP_PRELUDE_USE_TEXT_FORMATTER";

let consoleUseJsonFormatter = lazy (
    let v = System.Environment.GetEnvironmentVariable DAP_PRELUDE_CONSOLE_USE_JSON_FORMATTER
    v <> null && v.Trim().ToLower() = "true"
)

let fileUseTextFormatter = lazy (
    let v = System.Environment.GetEnvironmentVariable DAP_PRELUDE_FILE_USE_TEXT_FORMATTER
    v <> null && v.Trim().ToLower() = "true"
)

[<Literal>]
let TextOutputTemplate = "{Timestamp:HH:mm:ss.fff} {Level:u3} <{Context}> {Message:lj}{NewLine}{Exception}"

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

let inline private write (logger : Serilog.ILogger) (mainLogger : Serilog.ILogger option)
        (level' : LogLevel) (format : string) (params' : obj list) =
    let level = level'.ToSerilogLevel
    let params' = List.toArray params'
    if level'.ToInt >= LogLevelError.ToInt then
        mainLogger
        |> Option.iter (fun l ->
            l.Write(level, format, params')
        )
    logger.Write(level, format, params')

let inline private write' (logger : Serilog.ILogger) (mainLogger : Serilog.ILogger option)
        (level' : LogLevel) (format : string) (params' : obj list) (e : exn) =
    let level = level'.ToSerilogLevel
    let params' = List.toArray params'
    if level'.ToInt >= LogLevelError.ToInt then
        mainLogger
        |> Option.iter (fun l ->
            l.Write(level, e, format, params')
        )
    logger.Write(level, e, format, params')

let private toSerilog (logger : Serilog.ILogger) (mainLogger : Serilog.ILogger option) (evt : LogEvent) =
    match evt.Exception with
    | None ->
        match evt.Level with
        | LogLevelFatal | LogLevelError ->
            let stackTrace = (System.Diagnostics.StackTrace(2)).ToString()
            let format = evt.Format + "\n{StackTrace}"
            let params' = evt.Params @ [stackTrace]
            write logger mainLogger evt.Level format params'
        | _ ->
            write logger mainLogger evt.Level evt.Format evt.Params
    | Some e ->
        write' logger mainLogger evt.Level evt.Format evt.Params e

let isSilentLogger (logger : Serilog.ILogger) =
    (* Not sure why can't access SilentLogger
    return logger :? Serilog.Core.Pipeline.SilentLogger
     *)
    not <| logger.IsEnabled (Serilog.Events.LogEventLevel.Fatal)

let forContext (context : string) (logger : Serilog.ILogger) =
    logger.ForContext("Context", context)

type SerilogLogger (target' : Serilog.ILogger, mainTarget' : Serilog.ILogger option) =
    let target = target'
    let mainTarget = mainTarget'
    member __.IsMain = mainTarget.IsNone
    member __.Log (evt : LogEvent) =
        toSerilog target mainTarget evt
    member __.Target = target
    member __.MainTarget = mainTarget
    member __.ForContext (context : string) =
        new SerilogLogger
            (
                forContext context target,
                mainTarget
                |> Option.map (forContext context)
            )
    interface ILogger with
        member this.Log (evt : LogEvent) =
            this.Log evt

type AddSink = Serilog.LoggerConfiguration -> Serilog.LoggerConfiguration

let private getMainTargetForLogging (logger : Serilog.ILogger) =
    if Serilog.Log.Logger =? logger then
        None
    elif isSilentLogger Serilog.Log.Logger then
        None
    else
        Some Serilog.Log.Logger

type SerilogLogging (target' : Serilog.ILogger) =
    let logger = new SerilogLogger (target', getMainTargetForLogging target')
    member __.IsMain =
        logger.IsMain
    member __.Close () : unit =
        if logger.IsMain then
            Serilog.Log.CloseAndFlush ()
        else
            (logger.Target :?> IDisposable) .Dispose ()
    static member Create (sinks : AddSink list) =
        let config = Serilog.LoggerConfiguration ()
        let config = config.Enrich.WithThreadId ()
        //let config = config.Enrich.WithDemystifiedStackTraces ()
        let config = sinks |> List.fold (fun c addSink -> addSink c) config
        let logger = config.CreateLogger()
        if isSilentLogger Serilog.Log.Logger then
            Serilog.Log.Logger <- logger
        new SerilogLogging (logger)
    interface ILogging with
        member this.Close () = this.Close ()
        member __.GetLogger (context : string) : ILogger =
            logger.ForContext context
            :> ILogger
    interface ILogger with
        member __.Log (evt : LogEvent) =
            logger.Log evt

let setupSerilog (sinks : AddSink list) : SerilogLogging =
    let logging = SerilogLogging.Create sinks
    if logging.IsMain then
        logging
        |> setLogging'
        |> ignore
    logging

let addConsoleSink (minimumLevel : LogLevel) : AddSink =
    fun config ->
        let theme = Serilog.Sinks.SystemConsole.Themes.AnsiConsoleTheme.Code
        if consoleUseJsonFormatter.Force () then
            Serilog.ConsoleLoggerConfigurationExtensions.Console(config.WriteTo,
                formatter = Serilog.Formatting.Compact.CompactJsonFormatter(),
                restrictedToMinimumLevel = minimumLevel.ToSerilogLevel)
        else
            Serilog.ConsoleLoggerConfigurationExtensions.Console(config.WriteTo,
                restrictedToMinimumLevel = minimumLevel.ToSerilogLevel,
                outputTemplate = TextOutputTemplate,
                theme = theme)

let private checkDirectory (path : string) =
    let dirInfo = (new FileInfo (path)).Directory;
    if not dirInfo.Exists then
        dirInfo.Create();

let addRollingFileSink (rollingInterval : RollingInterval) (path : string) (minimumLevel : LogLevel) : AddSink =
    checkDirectory path
    fun config ->
        if fileUseTextFormatter.Force () then
            Serilog.FileLoggerConfigurationExtensions.File(config.WriteTo,
                path = path,
                outputTemplate = TextOutputTemplate,
                restrictedToMinimumLevel = minimumLevel.ToSerilogLevel,
                rollingInterval = rollingInterval)
        else
            Serilog.FileLoggerConfigurationExtensions.File(config.WriteTo,
                formatter = Serilog.Formatting.Compact.CompactJsonFormatter(),
                path = path,
                restrictedToMinimumLevel = minimumLevel.ToSerilogLevel,
                rollingInterval = rollingInterval)

let addDailyFileSink : string -> LogLevel -> AddSink =
    addRollingFileSink RollingInterval.Day

let addHourlyFileSink : string -> LogLevel -> AddSink =
    addRollingFileSink RollingInterval.Hour

let addFileSink : string -> LogLevel -> AddSink =
    addRollingFileSink RollingInterval.Infinite

let addSeqSink (uri : string) (minimumLevel : LogLevel) : AddSink =
    fun config ->
        Serilog.SeqLoggerConfigurationExtensions.Seq(config.WriteTo,
            uri,
            restrictedToMinimumLevel = minimumLevel.ToSerilogLevel)

let setupConsole (minimumLevel : LogLevel) =
    setupSerilog [ addConsoleSink minimumLevel ]

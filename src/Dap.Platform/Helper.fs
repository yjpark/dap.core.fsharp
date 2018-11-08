[<AutoOpen>]
module Dap.Platform.Helper

open System.IO
open Dap.Prelude

let inline addFutureCmd (delay : float<second>) (msg : 'msg) (runner : ^runner) ((model, cmd) : 'model * Cmd<'msg>) : 'model * Cmd<'msg> =
    let interval = 1000.0 * (float delay)
    let timer = new System.Timers.Timer(Interval = interval, Enabled = true, AutoReset = false)
    timer.Elapsed.AddHandler(new System.Timers.ElapsedEventHandler(fun _src _evt ->
        (^runner : (member Deliver : 'msg -> unit) (runner, msg))
    ))
    (model, cmd)

let calcSha256Sum (content : string) : string =
    if content = "" || content =? null then
        ""
    else
        use sha256 = System.Security.Cryptography.SHA256.Create()
        let hash = sha256.ComputeHash (System.Text.Encoding.UTF8.GetBytes content)
        System.Convert.ToBase64String hash

let calcSha256SumWithSalt (salt : string) (content : string) : string =
    calcSha256Sum <| content + salt

let checkDirectory (runner : IRunner) (path : string) (section : string) =
    let dirInfo = (new FileInfo (path)).Directory;
    if not dirInfo.Exists then
        dirInfo.Create();
        logInfo runner section "Directory_Created" dirInfo

let calcSha256Sum2 (content : string) : string =
    use sha256 = System.Security.Cryptography.SHA256.Create()
    let hash = sha256.ComputeHash (System.Text.Encoding.UTF8.GetBytes content)
    hash
    |> Array.map (fun b -> b.ToString "x2")
    |> String.concat ""

let calcSha256Sum2WithSalt (salt : string) (content : string) : string =
    calcSha256Sum2 <| content + salt

type ConsoleSinkArgs with
    member this.ToAddSink () =
        addConsoleSink this.MinLevel
    static member ConsoleProvider (this : ConsoleSinkArgs) =
        this.ToAddSink ()

type FileSinkArgs with
    member this.ToAddSink () =
        match this.Rolling with
        | None ->
            addFileSink this.Path this.MinLevel
        | Some RollingInterval.Daily ->
            addDailyFileSink this.Path this.MinLevel
        | Some RollingInterval.Hourly ->
            addHourlyFileSink this.Path this.MinLevel
    member this.Folder : string = System.IO.Path.GetDirectoryName this.Path
    member this.Filename : string = System.IO.Path.GetFileName this.Path
    member this.WithFolder (folder : string, ?noTimestamp : bool) =
        let noTimestamp = defaultArg noTimestamp false
        if noTimestamp then
            System.IO.Path.Combine (folder, this.Filename)
        else
            let timestamp = getNow' () |> instantToText
            let timestamp = timestamp.Replace (":", "_")
            System.IO.Path.Combine (folder, timestamp, this.Filename)
        |> this.WithPath
    member this.WithFilename (filename : string) =
        System.IO.Path.Combine (this.Folder, filename)
        |> this.WithPath

type LoggingArgs with
    static member CreateBoth (logFile : string, ?fileMinLevel : LogLevel, ?fileRollingInterval : RollingInterval, ?consoleMinLevel : LogLevel) : LoggingArgs =
        let consoleMinLevel = defaultArg consoleMinLevel LogLevelWarning
        let fileMinLevel = defaultArg fileMinLevel LogLevelInformation
        let fileRollingInterval = defaultArg fileRollingInterval RollingInterval.Daily
        LoggingArgs.Create (
            console = ConsoleSinkArgs.Create (consoleMinLevel),
            file = FileSinkArgs.Create (logFile, fileMinLevel, fileRollingInterval)
        )
    member this.ToSerilogLogging (?consoleProvider : ConsoleSinkArgs -> AddSink) : SerilogLogging =
        let consoleProvider = defaultArg consoleProvider ConsoleSinkArgs.ConsoleProvider
        [
            if this.Console.IsSome then
                yield consoleProvider this.Console.Value
            if this.File.IsSome then
                yield this.File.Value.ToAddSink ()
        ]|> (fun sinks ->
            if sinks.Length = 0 then
                failWith "LoggingArgs" "No_Sinks"
            sinks
        )|> setupSerilog
    member this.WithFolder (folder : string, ?noTimestamp : bool) =
        this.File
        |> Option.map (fun f -> f.WithFolder (folder, ?noTimestamp = noTimestamp))
        |> this.WithFile
    member this.WithFilename (filename : string) =
        this.File
        |> Option.map (fun f -> f.WithFilename (filename))
        |> this.WithFile

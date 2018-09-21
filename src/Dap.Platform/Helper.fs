[<AutoOpen>]
module Dap.Platform.Helper

open System.IO
open Dap.Prelude
open Serilog

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

type FileSinkArgs with
    member this.ToAddSink () =
        match this.Rolling with
        | None ->
            addFileSink this.Path this.MinLevel
        | Some RollingInterval.Daily ->
            addDailyFileSink this.Path this.MinLevel
        | Some RollingInterval.Hourly ->
            addHourlyFileSink this.Path this.MinLevel
    member this.LogFolder = System.IO.Path.GetDirectoryName this.Path

type LoggingArgs with
    member this.CreateLogging () : SerilogLogging =
        [
            if this.Console.IsSome then
                yield this.Console.Value.ToAddSink ()
            if this.File.IsSome then
                yield this.File.Value.ToAddSink ()
        ]|> (fun sinks ->
            if sinks.Length = 0 then
                failWith "LoggingArgs" "No_Sinks"
            sinks
        )|> setupSerilog
    member this.WithConsoleMinLevel minLevel =
        this.Console
        |> Option.map (fun x -> x.WithMinLevel minLevel)
        |> this.WithConsole
    member this.WithFileMinLevel minLevel =
        this.File
        |> Option.map (fun x -> x.WithMinLevel minLevel)
        |> this.WithFile
    member this.LogFolder =
        this.File
        |> Option.map (fun x -> x.LogFolder)
        |> Option.defaultValue ""
    member this.WithFileName filename =
        this.File
        |> Option.map (fun x ->
            x.WithPath <| System.IO.Path.Combine (x.LogFolder, filename)
        )|> this.WithFile

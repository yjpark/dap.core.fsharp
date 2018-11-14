module Dap.Platform.ArgsBuilder

open Dap.Prelude
open Dap.Context
open Dap.Context.Builder

(*
 * Generated: <ValueBuilder>
 *)
type ConsoleSinkArgsBuilder
        (
            minLevel : (* ConsoleSinkArgsBuilder *) LogLevel
        ) =
    inherit ObjBuilder<ConsoleSinkArgs> ()
    override __.Zero () =
        ConsoleSinkArgs.Create (
            minLevel = (* ConsoleSinkArgsBuilder *) minLevel
        )
    [<CustomOperation("min_level")>]
    member __.MinLevel (target : ConsoleSinkArgs, (* ConsoleSinkArgs *) minLevel : LogLevel) =
        target.WithMinLevel minLevel

let console_sink_args minLevel =
    new ConsoleSinkArgsBuilder (minLevel)

(*
 * Generated: <ValueBuilder>
 *)
type FileSinkArgsBuilder
        (
            path : (* FileSinkArgsBuilder *) string
        ) =
    inherit ObjBuilder<FileSinkArgs> ()
    override __.Zero () =
        FileSinkArgs.Create (
            path = (* FileSinkArgsBuilder *) path
        )
    [<CustomOperation("path")>]
    member __.Path (target : FileSinkArgs, (* FileSinkArgs *) path : string) =
        target.WithPath path
    [<CustomOperation("min_level")>]
    member __.MinLevel (target : FileSinkArgs, (* FileSinkArgs *) minLevel : LogLevel) =
        target.WithMinLevel minLevel
    [<CustomOperation("rolling")>]
    member __.Rolling (target : FileSinkArgs, (* FileSinkArgs *) rolling : RollingInterval option) =
        target.WithRolling rolling

let file_sink_args path =
    new FileSinkArgsBuilder (path)

(*
 * Generated: <ValueBuilder>
 *)
type LoggingArgsBuilder () =
    inherit ObjBuilder<LoggingArgs> ()
    override __.Zero () = LoggingArgs.Create ()
    [<CustomOperation("console")>]
    member __.Console (target : LoggingArgs, (* LoggingArgs *) console : ConsoleSinkArgs option) =
        target.WithConsole console
    [<CustomOperation("file")>]
    member __.File (target : LoggingArgs, (* LoggingArgs *) file : FileSinkArgs option) =
        target.WithFile file

let logging_args = new LoggingArgsBuilder ()

(*
 * Generated: <ValueBuilder>
 *)
type TickerArgsBuilder () =
    inherit ObjBuilder<TickerArgs> ()
    override __.Zero () = TickerArgs.Create ()
    [<CustomOperation("frame_rate")>]
    member __.FrameRate (target : TickerArgs, (* TickerArgs *) frameRate : float) =
        target.WithFrameRate frameRate
    [<CustomOperation("auto_start")>]
    member __.AutoStart (target : TickerArgs, (* TickerArgs *) autoStart : bool) =
        target.WithAutoStart autoStart

let ticker_args = new TickerArgsBuilder ()
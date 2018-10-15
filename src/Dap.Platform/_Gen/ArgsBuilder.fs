module Dap.Platform.ArgsBuilder

open Dap.Prelude
open Dap.Context
open Dap.Context.Builder

(*
 * Generated: <ValueBuilder>
 *)
type ConsoleSinkArgsBuilder () =
    inherit ObjBuilder<ConsoleSinkArgs> ()
    override __.Zero () = ConsoleSinkArgs.Default ()
    [<CustomOperation("min_level")>]
    member __.MinLevel (target : ConsoleSinkArgs, (* ConsoleSinkArgs *) minLevel : LogLevel) =
        target.WithMinLevel minLevel

let console_sink_args = ConsoleSinkArgsBuilder ()

(*
 * Generated: <ValueBuilder>
 *)
type FileSinkArgsBuilder () =
    inherit ObjBuilder<FileSinkArgs> ()
    override __.Zero () = FileSinkArgs.Default ()
    [<CustomOperation("min_level")>]
    member __.MinLevel (target : FileSinkArgs, (* FileSinkArgs *) minLevel : LogLevel) =
        target.WithMinLevel minLevel
    [<CustomOperation("path")>]
    member __.Path (target : FileSinkArgs, (* FileSinkArgs *) path : string) =
        target.WithPath path
    [<CustomOperation("rolling")>]
    member __.Rolling (target : FileSinkArgs, (* FileSinkArgs *) rolling : RollingInterval option) =
        target.WithRolling rolling

let file_sink_args = FileSinkArgsBuilder ()

(*
 * Generated: <ValueBuilder>
 *)
type LoggingArgsBuilder () =
    inherit ObjBuilder<LoggingArgs> ()
    override __.Zero () = LoggingArgs.Default ()
    [<CustomOperation("console")>]
    member __.Console (target : LoggingArgs, (* LoggingArgs *) console : ConsoleSinkArgs option) =
        target.WithConsole console
    [<CustomOperation("file")>]
    member __.File (target : LoggingArgs, (* LoggingArgs *) file : FileSinkArgs option) =
        target.WithFile file

let logging_args = LoggingArgsBuilder ()

(*
 * Generated: <ValueBuilder>
 *)
type TickerArgsBuilder () =
    inherit ObjBuilder<TickerArgs> ()
    override __.Zero () = TickerArgs.Default ()
    [<CustomOperation("frame_rate")>]
    member __.FrameRate (target : TickerArgs, (* TickerArgs *) frameRate : float) =
        target.WithFrameRate frameRate
    [<CustomOperation("auto_start")>]
    member __.AutoStart (target : TickerArgs, (* TickerArgs *) autoStart : bool) =
        target.WithAutoStart autoStart

let ticker_args = TickerArgsBuilder ()
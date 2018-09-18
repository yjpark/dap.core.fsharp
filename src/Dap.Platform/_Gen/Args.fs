[<AutoOpen>]
module Dap.Platform.Args

open Dap.Prelude
open Dap.Context

(*
 * Generated: <Record>
 *     IsJson, IsLoose
 *)
type ConsoleSinkArgs = {
    MinLevel : LogLevel
} with
    static member Create minLevel
            : ConsoleSinkArgs =
        {
            MinLevel = minLevel
        }
    static member Default () =
        ConsoleSinkArgs.Create
            LogLevelWarning
    static member JsonEncoder : JsonEncoder<ConsoleSinkArgs> =
        fun (this : ConsoleSinkArgs) ->
            E.object [
                "min_level", LogLevel.JsonEncoder this.MinLevel
            ]
    static member JsonDecoder : JsonDecoder<ConsoleSinkArgs> =
        D.decode ConsoleSinkArgs.Create
        |> D.optional "min_level" LogLevel.JsonDecoder LogLevelWarning
    static member JsonSpec =
        FieldSpec.Create<ConsoleSinkArgs>
            ConsoleSinkArgs.JsonEncoder ConsoleSinkArgs.JsonDecoder
    interface IJson with
        member this.ToJson () = ConsoleSinkArgs.JsonEncoder this
    interface IObj
    member this.WithMinLevel (minLevel : LogLevel) = {this with MinLevel = minLevel}

(*
 * Generated: <Union>
 *     IsJson
 *)
type RollingInterval =
    | Daily
    | Hourly
with
    static member CreateDaily () : RollingInterval =
        Daily
    static member CreateHourly () : RollingInterval =
        Hourly
    static member JsonSpec' : CaseSpec<RollingInterval> list =
        [
            CaseSpec<RollingInterval>.Create "Daily" []
            CaseSpec<RollingInterval>.Create "Hourly" []
        ]
    static member JsonEncoder = E.union RollingInterval.JsonSpec'
    static member JsonDecoder = D.union RollingInterval.JsonSpec'
    static member JsonSpec =
        FieldSpec.Create<RollingInterval>
            RollingInterval.JsonEncoder RollingInterval.JsonDecoder
    interface IJson with
        member this.ToJson () = RollingInterval.JsonEncoder this

(*
 * Generated: <Record>
 *     IsJson, IsLoose
 *)
type FileSinkArgs = {
    MinLevel : LogLevel
    Path : string
    Rolling : RollingInterval option
} with
    static member Create minLevel path rolling
            : FileSinkArgs =
        {
            MinLevel = minLevel
            Path = path
            Rolling = rolling
        }
    static member Default () =
        FileSinkArgs.Create
            LogLevelInformation
            ""
            None
    static member JsonEncoder : JsonEncoder<FileSinkArgs> =
        fun (this : FileSinkArgs) ->
            E.object [
                "min_level", LogLevel.JsonEncoder this.MinLevel
                "path", E.string this.Path
                "rolling", (E.option RollingInterval.JsonEncoder) this.Rolling
            ]
    static member JsonDecoder : JsonDecoder<FileSinkArgs> =
        D.decode FileSinkArgs.Create
        |> D.optional "min_level" LogLevel.JsonDecoder LogLevelInformation
        |> D.optional "path" D.string ""
        |> D.optional "rolling" (D.option RollingInterval.JsonDecoder) None
    static member JsonSpec =
        FieldSpec.Create<FileSinkArgs>
            FileSinkArgs.JsonEncoder FileSinkArgs.JsonDecoder
    interface IJson with
        member this.ToJson () = FileSinkArgs.JsonEncoder this
    interface IObj
    member this.WithMinLevel (minLevel : LogLevel) = {this with MinLevel = minLevel}
    member this.WithPath (path : string) = {this with Path = path}
    member this.WithRolling (rolling : RollingInterval option) = {this with Rolling = rolling}

(*
 * Generated: <Record>
 *     IsJson, IsLoose
 *)
type LoggingArgs = {
    Console : ConsoleSinkArgs option
    File : FileSinkArgs option
} with
    static member Create console file
            : LoggingArgs =
        {
            Console = console
            File = file
        }
    static member Default () =
        LoggingArgs.Create
            None
            None
    static member JsonEncoder : JsonEncoder<LoggingArgs> =
        fun (this : LoggingArgs) ->
            E.object [
                "console", (E.option ConsoleSinkArgs.JsonEncoder) this.Console
                "file", (E.option FileSinkArgs.JsonEncoder) this.File
            ]
    static member JsonDecoder : JsonDecoder<LoggingArgs> =
        D.decode LoggingArgs.Create
        |> D.optional "console" (D.option ConsoleSinkArgs.JsonDecoder) None
        |> D.optional "file" (D.option FileSinkArgs.JsonDecoder) None
    static member JsonSpec =
        FieldSpec.Create<LoggingArgs>
            LoggingArgs.JsonEncoder LoggingArgs.JsonDecoder
    interface IJson with
        member this.ToJson () = LoggingArgs.JsonEncoder this
    interface IObj
    member this.WithConsole (console : ConsoleSinkArgs option) = {this with Console = console}
    member this.WithFile (file : FileSinkArgs option) = {this with File = file}

(*
 * Generated: <Record>
 *     IsJson, IsLoose
 *)
type TickerArgs = {
    FrameRate : float
    AutoStart : bool
} with
    static member Create frameRate autoStart
            : TickerArgs =
        {
            FrameRate = frameRate
            AutoStart = autoStart
        }
    static member Default () =
        TickerArgs.Create
            10.0
            true
    static member JsonEncoder : JsonEncoder<TickerArgs> =
        fun (this : TickerArgs) ->
            E.object [
                "frame_rate", E.float this.FrameRate
                "auto_start", E.bool this.AutoStart
            ]
    static member JsonDecoder : JsonDecoder<TickerArgs> =
        D.decode TickerArgs.Create
        |> D.optional "frame_rate" D.float 10.0
        |> D.optional "auto_start" D.bool true
    static member JsonSpec =
        FieldSpec.Create<TickerArgs>
            TickerArgs.JsonEncoder TickerArgs.JsonDecoder
    interface IJson with
        member this.ToJson () = TickerArgs.JsonEncoder this
    interface IObj
    member this.WithFrameRate (frameRate : float) = {this with FrameRate = frameRate}
    member this.WithAutoStart (autoStart : bool) = {this with AutoStart = autoStart}
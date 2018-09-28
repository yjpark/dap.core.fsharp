[<AutoOpen>]
module Dap.Platform.Args

open Dap.Prelude
open Dap.Context
open Dap.Context.Helper

(*
 * Generated: <Record>
 *     IsJson, IsLoose
 *)
type ConsoleSinkArgs = {
    MinLevel : (* ConsoleSinkArgs *) LogLevel
} with
    static member Create minLevel
            : ConsoleSinkArgs =
        {
            MinLevel = minLevel
        }
    static member Default () =
        ConsoleSinkArgs.Create
            LogLevelWarning
    static member SetMinLevel (minLevel : LogLevel) (this : ConsoleSinkArgs) =
        {this with MinLevel = minLevel}
    static member UpdateMinLevel (update : LogLevel -> LogLevel) (this : ConsoleSinkArgs) =
        this |> ConsoleSinkArgs.SetMinLevel (update this.MinLevel)
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
    member this.WithMinLevel ((* ConsoleSinkArgs *) minLevel : LogLevel) =
        this |> ConsoleSinkArgs.SetMinLevel minLevel

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
    MinLevel : (* FileSinkArgs *) LogLevel
    Path : (* FileSinkArgs *) string
    Rolling : (* FileSinkArgs *) RollingInterval option
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
    static member SetMinLevel (minLevel : LogLevel) (this : FileSinkArgs) =
        {this with MinLevel = minLevel}
    static member SetPath (path : string) (this : FileSinkArgs) =
        {this with Path = path}
    static member SetRolling (rolling : RollingInterval option) (this : FileSinkArgs) =
        {this with Rolling = rolling}
    static member UpdateMinLevel (update : LogLevel -> LogLevel) (this : FileSinkArgs) =
        this |> FileSinkArgs.SetMinLevel (update this.MinLevel)
    static member UpdatePath (update : string -> string) (this : FileSinkArgs) =
        this |> FileSinkArgs.SetPath (update this.Path)
    static member UpdateRolling (update : RollingInterval option -> RollingInterval option) (this : FileSinkArgs) =
        this |> FileSinkArgs.SetRolling (update this.Rolling)
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
    member this.WithMinLevel ((* FileSinkArgs *) minLevel : LogLevel) =
        this |> FileSinkArgs.SetMinLevel minLevel
    member this.WithPath ((* FileSinkArgs *) path : string) =
        this |> FileSinkArgs.SetPath path
    member this.WithRolling ((* FileSinkArgs *) rolling : RollingInterval option) =
        this |> FileSinkArgs.SetRolling rolling

(*
 * Generated: <Record>
 *     IsJson, IsLoose
 *)
type LoggingArgs = {
    Console : (* LoggingArgs *) ConsoleSinkArgs option
    File : (* LoggingArgs *) FileSinkArgs option
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
    static member SetConsole (console : ConsoleSinkArgs option) (this : LoggingArgs) =
        {this with Console = console}
    static member SetFile (file : FileSinkArgs option) (this : LoggingArgs) =
        {this with File = file}
    static member UpdateConsole (update : ConsoleSinkArgs option -> ConsoleSinkArgs option) (this : LoggingArgs) =
        this |> LoggingArgs.SetConsole (update this.Console)
    static member UpdateFile (update : FileSinkArgs option -> FileSinkArgs option) (this : LoggingArgs) =
        this |> LoggingArgs.SetFile (update this.File)
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
    member this.WithConsole ((* LoggingArgs *) console : ConsoleSinkArgs option) =
        this |> LoggingArgs.SetConsole console
    member this.WithFile ((* LoggingArgs *) file : FileSinkArgs option) =
        this |> LoggingArgs.SetFile file

(*
 * Generated: <Record>
 *     IsJson, IsLoose
 *)
type TickerArgs = {
    FrameRate : (* TickerArgs *) float
    AutoStart : (* TickerArgs *) bool
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
    static member SetFrameRate (frameRate : float) (this : TickerArgs) =
        {this with FrameRate = frameRate}
    static member SetAutoStart (autoStart : bool) (this : TickerArgs) =
        {this with AutoStart = autoStart}
    static member UpdateFrameRate (update : float -> float) (this : TickerArgs) =
        this |> TickerArgs.SetFrameRate (update this.FrameRate)
    static member UpdateAutoStart (update : bool -> bool) (this : TickerArgs) =
        this |> TickerArgs.SetAutoStart (update this.AutoStart)
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
    member this.WithFrameRate ((* TickerArgs *) frameRate : float) =
        this |> TickerArgs.SetFrameRate frameRate
    member this.WithAutoStart ((* TickerArgs *) autoStart : bool) =
        this |> TickerArgs.SetAutoStart autoStart
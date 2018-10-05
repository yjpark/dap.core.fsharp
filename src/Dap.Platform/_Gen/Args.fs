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
            MinLevel = (* ConsoleSinkArgs *) minLevel
        }
    static member Default () =
        ConsoleSinkArgs.Create
            LogLevelWarning (* ConsoleSinkArgs *) (* minLevel *)
    static member SetMinLevel ((* ConsoleSinkArgs *) minLevel : LogLevel) (this : ConsoleSinkArgs) =
        {this with MinLevel = minLevel}
    static member UpdateMinLevel ((* ConsoleSinkArgs *) update : LogLevel -> LogLevel) (this : ConsoleSinkArgs) =
        this |> ConsoleSinkArgs.SetMinLevel (update this.MinLevel)
    static member JsonEncoder : JsonEncoder<ConsoleSinkArgs> =
        fun (this : ConsoleSinkArgs) ->
            E.object [
                "min_level", LogLevel.JsonEncoder (* ConsoleSinkArgs *) this.MinLevel
            ]
    static member JsonDecoder : JsonDecoder<ConsoleSinkArgs> =
        D.object (fun get ->
            {
                MinLevel = get.Optional.Field (* ConsoleSinkArgs *) "min_level" LogLevel.JsonDecoder
                    |> Option.defaultValue LogLevelWarning
            }
        )
    static member JsonSpec =
        FieldSpec.Create<ConsoleSinkArgs> ConsoleSinkArgs.JsonEncoder ConsoleSinkArgs.JsonDecoder
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
            MinLevel = (* FileSinkArgs *) minLevel
            Path = (* FileSinkArgs *) path
            Rolling = (* FileSinkArgs *) rolling
        }
    static member Default () =
        FileSinkArgs.Create
            LogLevelInformation (* FileSinkArgs *) (* minLevel *)
            "" (* FileSinkArgs *) (* path *)
            None (* FileSinkArgs *) (* rolling *)
    static member SetMinLevel ((* FileSinkArgs *) minLevel : LogLevel) (this : FileSinkArgs) =
        {this with MinLevel = minLevel}
    static member SetPath ((* FileSinkArgs *) path : string) (this : FileSinkArgs) =
        {this with Path = path}
    static member SetRolling ((* FileSinkArgs *) rolling : RollingInterval option) (this : FileSinkArgs) =
        {this with Rolling = rolling}
    static member UpdateMinLevel ((* FileSinkArgs *) update : LogLevel -> LogLevel) (this : FileSinkArgs) =
        this |> FileSinkArgs.SetMinLevel (update this.MinLevel)
    static member UpdatePath ((* FileSinkArgs *) update : string -> string) (this : FileSinkArgs) =
        this |> FileSinkArgs.SetPath (update this.Path)
    static member UpdateRolling ((* FileSinkArgs *) update : RollingInterval option -> RollingInterval option) (this : FileSinkArgs) =
        this |> FileSinkArgs.SetRolling (update this.Rolling)
    static member JsonEncoder : JsonEncoder<FileSinkArgs> =
        fun (this : FileSinkArgs) ->
            E.object [
                "min_level", LogLevel.JsonEncoder (* FileSinkArgs *) this.MinLevel
                "path", E.string (* FileSinkArgs *) this.Path
                "rolling", (E.option RollingInterval.JsonEncoder) (* FileSinkArgs *) this.Rolling
            ]
    static member JsonDecoder : JsonDecoder<FileSinkArgs> =
        D.object (fun get ->
            {
                MinLevel = get.Optional.Field (* FileSinkArgs *) "min_level" LogLevel.JsonDecoder
                    |> Option.defaultValue LogLevelInformation
                Path = get.Optional.Field (* FileSinkArgs *) "path" D.string
                    |> Option.defaultValue ""
                Rolling = get.Optional.Field (* FileSinkArgs *) "rolling" RollingInterval.JsonDecoder
            }
        )
    static member JsonSpec =
        FieldSpec.Create<FileSinkArgs> FileSinkArgs.JsonEncoder FileSinkArgs.JsonDecoder
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
            Console = (* LoggingArgs *) console
            File = (* LoggingArgs *) file
        }
    static member Default () =
        LoggingArgs.Create
            None (* LoggingArgs *) (* console *)
            None (* LoggingArgs *) (* file *)
    static member SetConsole ((* LoggingArgs *) console : ConsoleSinkArgs option) (this : LoggingArgs) =
        {this with Console = console}
    static member SetFile ((* LoggingArgs *) file : FileSinkArgs option) (this : LoggingArgs) =
        {this with File = file}
    static member UpdateConsole ((* LoggingArgs *) update : ConsoleSinkArgs option -> ConsoleSinkArgs option) (this : LoggingArgs) =
        this |> LoggingArgs.SetConsole (update this.Console)
    static member UpdateFile ((* LoggingArgs *) update : FileSinkArgs option -> FileSinkArgs option) (this : LoggingArgs) =
        this |> LoggingArgs.SetFile (update this.File)
    static member JsonEncoder : JsonEncoder<LoggingArgs> =
        fun (this : LoggingArgs) ->
            E.object [
                "console", (E.option ConsoleSinkArgs.JsonEncoder) (* LoggingArgs *) this.Console
                "file", (E.option FileSinkArgs.JsonEncoder) (* LoggingArgs *) this.File
            ]
    static member JsonDecoder : JsonDecoder<LoggingArgs> =
        D.object (fun get ->
            {
                Console = get.Optional.Field (* LoggingArgs *) "console" ConsoleSinkArgs.JsonDecoder
                File = get.Optional.Field (* LoggingArgs *) "file" FileSinkArgs.JsonDecoder
            }
        )
    static member JsonSpec =
        FieldSpec.Create<LoggingArgs> LoggingArgs.JsonEncoder LoggingArgs.JsonDecoder
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
            FrameRate = (* TickerArgs *) frameRate
            AutoStart = (* TickerArgs *) autoStart
        }
    static member Default () =
        TickerArgs.Create
            10.0 (* TickerArgs *) (* frameRate *)
            true (* TickerArgs *) (* autoStart *)
    static member SetFrameRate ((* TickerArgs *) frameRate : float) (this : TickerArgs) =
        {this with FrameRate = frameRate}
    static member SetAutoStart ((* TickerArgs *) autoStart : bool) (this : TickerArgs) =
        {this with AutoStart = autoStart}
    static member UpdateFrameRate ((* TickerArgs *) update : float -> float) (this : TickerArgs) =
        this |> TickerArgs.SetFrameRate (update this.FrameRate)
    static member UpdateAutoStart ((* TickerArgs *) update : bool -> bool) (this : TickerArgs) =
        this |> TickerArgs.SetAutoStart (update this.AutoStart)
    static member JsonEncoder : JsonEncoder<TickerArgs> =
        fun (this : TickerArgs) ->
            E.object [
                "frame_rate", E.float (* TickerArgs *) this.FrameRate
                "auto_start", E.bool (* TickerArgs *) this.AutoStart
            ]
    static member JsonDecoder : JsonDecoder<TickerArgs> =
        D.object (fun get ->
            {
                FrameRate = get.Optional.Field (* TickerArgs *) "frame_rate" D.float
                    |> Option.defaultValue 10.0
                AutoStart = get.Optional.Field (* TickerArgs *) "auto_start" D.bool
                    |> Option.defaultValue true
            }
        )
    static member JsonSpec =
        FieldSpec.Create<TickerArgs> TickerArgs.JsonEncoder TickerArgs.JsonDecoder
    interface IJson with
        member this.ToJson () = TickerArgs.JsonEncoder this
    interface IObj
    member this.WithFrameRate ((* TickerArgs *) frameRate : float) =
        this |> TickerArgs.SetFrameRate frameRate
    member this.WithAutoStart ((* TickerArgs *) autoStart : bool) =
        this |> TickerArgs.SetAutoStart autoStart
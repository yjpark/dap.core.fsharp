[<AutoOpen>]
module Dap.Archive.Args

open Dap.Prelude
open Dap.Context
open Dap.Platform

(*
 * Generated: <Record>
 *     IsJson, IsLoose
 *)
type RecorderArgs = {
    TickerKind : string
    TickerKey : string
    FlushInterval : Duration
} with
    static member Create tickerKind tickerKey flushInterval
            : RecorderArgs =
        {
            TickerKind = tickerKind
            TickerKey = tickerKey
            FlushInterval = flushInterval
        }
    static member Default () =
        RecorderArgs.Create
            "Ticker"
            ""
            (decodeJsonString Duration.JsonDecoder """0:00:00:30""")
    static member SetTickerKind (tickerKind : string) (this : RecorderArgs) =
        {this with TickerKind = tickerKind}
    static member SetTickerKey (tickerKey : string) (this : RecorderArgs) =
        {this with TickerKey = tickerKey}
    static member SetFlushInterval (flushInterval : Duration) (this : RecorderArgs) =
        {this with FlushInterval = flushInterval}
    static member UpdateTickerKind (update : string -> string) (this : RecorderArgs) =
        this |> RecorderArgs.SetTickerKind (update this.TickerKind)
    static member UpdateTickerKey (update : string -> string) (this : RecorderArgs) =
        this |> RecorderArgs.SetTickerKey (update this.TickerKey)
    static member UpdateFlushInterval (update : Duration -> Duration) (this : RecorderArgs) =
        this |> RecorderArgs.SetFlushInterval (update this.FlushInterval)
    static member JsonEncoder : JsonEncoder<RecorderArgs> =
        fun (this : RecorderArgs) ->
            E.object [
                "ticker_kind", E.string this.TickerKind
                "ticker_key", E.string this.TickerKey
                "flush_interval", E.duration this.FlushInterval
            ]
    static member JsonDecoder : JsonDecoder<RecorderArgs> =
        D.decode RecorderArgs.Create
        |> D.optional "ticker_kind" D.string "Ticker"
        |> D.optional "ticker_key" D.string ""
        |> D.optional "flush_interval" D.duration (decodeJsonString Duration.JsonDecoder """0:00:00:30""")
    static member JsonSpec =
        FieldSpec.Create<RecorderArgs>
            RecorderArgs.JsonEncoder RecorderArgs.JsonDecoder
    interface IJson with
        member this.ToJson () = RecorderArgs.JsonEncoder this
    interface IObj
    member this.WithTickerKind (tickerKind : string) =
        this |> RecorderArgs.SetTickerKind tickerKind
    member this.WithTickerKey (tickerKey : string) =
        this |> RecorderArgs.SetTickerKey tickerKey
    member this.WithFlushInterval (flushInterval : Duration) =
        this |> RecorderArgs.SetFlushInterval flushInterval
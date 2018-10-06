[<AutoOpen>]
module Dap.Archive.Args

open Dap.Context.Helper
open Dap.Prelude
open Dap.Context
open Dap.Platform

(*
 * Generated: <Record>
 *     IsJson, IsLoose
 *)
type RecorderArgs = {
    TickerKind : (* RecorderArgs *) string
    TickerKey : (* RecorderArgs *) string
    FlushInterval : (* RecorderArgs *) Duration
} with
    static member Create tickerKind tickerKey flushInterval
            : RecorderArgs =
        {
            TickerKind = (* RecorderArgs *) tickerKind
            TickerKey = (* RecorderArgs *) tickerKey
            FlushInterval = (* RecorderArgs *) flushInterval
        }
    static member Default () =
        RecorderArgs.Create
            "Ticker" (* RecorderArgs *) (* tickerKind *)
            "" (* RecorderArgs *) (* tickerKey *)
            (decodeJsonString Duration.JsonDecoder """0:00:00:30""") (* RecorderArgs *) (* flushInterval *)
    static member SetTickerKind ((* RecorderArgs *) tickerKind : string) (this : RecorderArgs) =
        {this with TickerKind = tickerKind}
    static member SetTickerKey ((* RecorderArgs *) tickerKey : string) (this : RecorderArgs) =
        {this with TickerKey = tickerKey}
    static member SetFlushInterval ((* RecorderArgs *) flushInterval : Duration) (this : RecorderArgs) =
        {this with FlushInterval = flushInterval}
    static member UpdateTickerKind ((* RecorderArgs *) update : string -> string) (this : RecorderArgs) =
        this |> RecorderArgs.SetTickerKind (update this.TickerKind)
    static member UpdateTickerKey ((* RecorderArgs *) update : string -> string) (this : RecorderArgs) =
        this |> RecorderArgs.SetTickerKey (update this.TickerKey)
    static member UpdateFlushInterval ((* RecorderArgs *) update : Duration -> Duration) (this : RecorderArgs) =
        this |> RecorderArgs.SetFlushInterval (update this.FlushInterval)
    static member JsonEncoder : JsonEncoder<RecorderArgs> =
        fun (this : RecorderArgs) ->
            E.object [
                "ticker_kind", E.string (* RecorderArgs *) this.TickerKind
                "ticker_key", E.string (* RecorderArgs *) this.TickerKey
                "flush_interval", E.duration (* RecorderArgs *) this.FlushInterval
            ]
    static member JsonDecoder : JsonDecoder<RecorderArgs> =
        D.object (fun get ->
            {
                TickerKind = get.Optional.Field (* RecorderArgs *) "ticker_kind" D.string
                    |> Option.defaultValue "Ticker"
                TickerKey = get.Optional.Field (* RecorderArgs *) "ticker_key" D.string
                    |> Option.defaultValue ""
                FlushInterval = get.Optional.Field (* RecorderArgs *) "flush_interval" D.duration
                    |> Option.defaultValue (decodeJsonString Duration.JsonDecoder """0:00:00:30""")
            }
        )
    static member JsonSpec =
        FieldSpec.Create<RecorderArgs> (RecorderArgs.JsonEncoder, RecorderArgs.JsonDecoder)
    interface IJson with
        member this.ToJson () = RecorderArgs.JsonEncoder this
    interface IObj
    member this.WithTickerKind ((* RecorderArgs *) tickerKind : string) =
        this |> RecorderArgs.SetTickerKind tickerKind
    member this.WithTickerKey ((* RecorderArgs *) tickerKey : string) =
        this |> RecorderArgs.SetTickerKey tickerKey
    member this.WithFlushInterval ((* RecorderArgs *) flushInterval : Duration) =
        this |> RecorderArgs.SetFlushInterval flushInterval
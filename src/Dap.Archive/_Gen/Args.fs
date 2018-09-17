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
    TickerIdent : Ident
    FlushInterval : Duration
} with
    static member Create tickerIdent flushInterval
            : RecorderArgs =
        {
            TickerIdent = tickerIdent
            FlushInterval = flushInterval
        }
    static member Default () =
        RecorderArgs.Create
            noIdent
            (decodeJson Duration.JsonDecoder "0:00:00:30")
    static member JsonEncoder : JsonEncoder<RecorderArgs> =
        fun (this : RecorderArgs) ->
            E.object [
                "ticker_ident", E.ident this.TickerIdent
                "flush_interval", E.duration this.FlushInterval
            ]
    static member JsonDecoder : JsonDecoder<RecorderArgs> =
        D.decode RecorderArgs.Create
        |> D.optional "ticker_ident" D.ident noIdent
        |> D.optional "flush_interval" D.duration (decodeJson Duration.JsonDecoder "0:00:00:30")
    static member JsonSpec =
        FieldSpec.Create<RecorderArgs>
            RecorderArgs.JsonEncoder RecorderArgs.JsonDecoder
    interface IJson with
        member this.ToJson () = RecorderArgs.JsonEncoder this
    interface IObj
    member this.WithTickerIdent (tickerIdent : Ident) = {this with TickerIdent = tickerIdent}
    member this.WithFlushInterval (flushInterval : Duration) = {this with FlushInterval = flushInterval}
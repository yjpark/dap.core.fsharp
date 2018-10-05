module Dap.Archive.ArgsBuilder

open Dap.Context.Helper
open Dap.Prelude
open Dap.Context
open Dap.Context.Builder
open Dap.Platform

(*
 * Generated: <ValueBuilder>
 *)
type RecorderArgsBuilder () =
    inherit ObjBuilder<RecorderArgs> ()
    override __.Zero () = RecorderArgs.Default ()
    [<CustomOperation("ticker_kind")>]
    member __.TickerKind (target : RecorderArgs, (* RecorderArgs *) tickerKind : string) =
        target.WithTickerKind tickerKind
    [<CustomOperation("ticker_key")>]
    member __.TickerKey (target : RecorderArgs, (* RecorderArgs *) tickerKey : string) =
        target.WithTickerKey tickerKey
    [<CustomOperation("flush_interval")>]
    member __.FlushInterval (target : RecorderArgs, (* RecorderArgs *) flushInterval : Duration) =
        target.WithFlushInterval flushInterval

let recorder_args = RecorderArgsBuilder ()
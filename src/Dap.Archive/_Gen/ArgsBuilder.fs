module Dap.Archive.ArgsBuilder

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
    [<CustomOperation("ticker_ident")>]
    member __.TickerIdent (target : RecorderArgs, tickerIdent : Ident) =
        target.WithTickerIdent tickerIdent
    [<CustomOperation("flush_interval")>]
    member __.FlushInterval (target : RecorderArgs, flushInterval : Duration) =
        target.WithFlushInterval flushInterval

let recorderArgs = RecorderArgsBuilder ()
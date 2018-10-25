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
    [<CustomOperation("flush_interval")>]
    member __.FlushInterval (target : RecorderArgs, (* RecorderArgs *) flushInterval : Duration) =
        target.WithFlushInterval flushInterval

let recorder_args = new RecorderArgsBuilder ()
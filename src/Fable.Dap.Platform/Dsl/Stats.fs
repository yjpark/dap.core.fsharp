module Dap.Platform.Dsl.Stats

open Dap.Prelude
open Dap.Context
open Dap.Context.Meta
open Dap.Context.Generator
open Dap.Platform

(*
 * Copied from Dap.Platform, change instant and duration type
 * This is only for decoding purpose to show data from non-fable side
 * It's not easy to remove the difference here, so keep the duplication
 * for now.
 *)

let OpLog =
    combo {
        var (M.string "op")
        var (M.string "msg")
        var (M.instant "time")
        var (M.duration "duration")
        var (M.string "stack_trace")
    }

let DurationStats =
    combo {
        var (M.duration "slow_cap")
        var (M.int "total_count")
        var (M.int "slow_count")
    }

let FuncStats =
    extend [ <@ DurationStats @> ] {
        var (M.int "pending_count")
        var (M.int "succeed_count")
        var (M.int "failed_count")
        list (M.custom (<@ OpLog @>, "failed_ops"))
    }

let Stats =
    combo {
        var (M.instant "time")
        prop (M.custom (<@ DurationStats @>, "deliver"))
        prop (M.custom (<@ DurationStats @>, "process"))
        prop (M.custom (<@ FuncStats @>, "reply"))
        prop (M.custom (<@ FuncStats @>, "func"))
        prop (M.custom (<@ FuncStats @>, "task"))
    }

let compile segments =
    [
        G.File (segments, ["_Gen"; "Stats.fs"],
            G.AutoOpenModule ("Dap.Platform.Stats",
                [
                    G.LooseJsonRecord (<@ OpLog @>)
                    G.FinalClass (<@ DurationStats @>)
                    G.FinalClass (<@ FuncStats @>)
                    G.BaseClass (<@ Stats @>)
                ]
            )
        )
    ]


module Dap.Platform.Dsl.Dash

open Dap.Prelude
open Dap.Context
open Dap.Context.Meta
open Dap.Context.Generator
open Dap.Platform

let OpLog =
    combo {
        var (M.string "op")
        var (M.string "msg")
        var (M.instant (InstantFormat.DateHourMinuteSecondSub, "time"))
        var (M.duration (DurationFormat.Second, "duration"))
        var (M.string "stack_trace")
    }

let DurationStats =
    combo {
        var (M.duration (DurationFormat.Second, "slow_cap", "DefaultSlowCap", ""))
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
        prop (M.custom (<@ DurationStats @>, "deliver"))
        prop (M.custom (<@ DurationStats @>, "process"))
        prop (M.custom (<@ FuncStats @>, "reply"))
        prop (M.custom (<@ FuncStats @>, "func"))
        prop (M.custom (<@ FuncStats @>, "task"))
    }

let DashProps =
    combo {
        var (M.instant (InstantFormat.DateHourMinuteSecondSub, "time"))
        var (M.json "model")
        prop (M.custom (<@ Stats @>, "stats"))
    }

let Dash =
    context <@ DashProps @> {
        handler (M.unit "inspect") (M.json response)
        handler (M.unit "clear_stats") (M.unit response)
    }

let compile segments =
    [
        G.File (segments, ["_Gen"; "Dash.fs"],
            G.AutoOpenModule ("Dap.Platform.Dash",
                [
                    G.LooseJsonRecord (<@ OpLog @>)
                    G.FinalCombo (<@ DurationStats @>)
                    G.FinalCombo (<@ FuncStats @>)
                    G.Combo (<@ Stats @>)
                    G.Combo (<@ DashProps @>)
                    G.Context (<@ Dash @>)
                ]
            )
        )
    ]


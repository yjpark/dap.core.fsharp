module Dap.WebSocket.Dsl

open Dap.Prelude
open Dap.Context
open Dap.Context.Meta
open Dap.Context.Generator
open Dap.Platform
open Dap.Platform.Meta
open Dap.Platform.Generator

let PktLog =
    combo {
        var (M.int "bytes")
        var (M.instant "time")
        var (M.duration (DurationFormat.Second, "duration"))
        var (M.string "stack_trace")
    }

let TrafficStats =
    combo {
        var (M.duration (DurationFormat.Second, "slow_cap", "DefaultPktSlowCap", ""))
        var (M.int "total_count")
        var (M.int "slow_count")
        var (M.int "pending_count")
        var (M.int "succeed_count")
        var (M.int "failed_count")
        list (M.custom (<@ PktLog @>, "failed_pkts"))
    }

let StatusLog =
    combo {
        var (M.instant "time")
        var (M.custom ("LinkStatus", "status"))
    }

let LinkStats =
    combo {
        var (M.custom (<@ StatusLog @>, "status"))
        list (M.custom (<@ StatusLog @>, "status_history"))
        prop (M.custom (<@ TrafficStats @>, "send"))
        prop (M.custom (<@ TrafficStats @>, "receive"))
    }

let compile segments =
    [
        G.File (segments, ["_Gen"; "Stats.fs"],
            G.AutoOpenModule ("Dap.WebSocket.Stats",
                [
                    G.PlatformOpens
                    G.LooseJsonRecord (<@ PktLog @>)
                    G.FinalClass (<@ TrafficStats @>)
                    G.LooseJsonRecord (<@ StatusLog @>)
                    G.FinalClass (<@ LinkStats @>)
                ]
            )
        )
    ]

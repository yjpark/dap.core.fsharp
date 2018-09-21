module Dap.Platform.Dsl

open Dap.Prelude
open Dap.Context
open Dap.Context.Meta
open Dap.Context.Generator

let ConsoleSinkArgs =
    combo {
        var (M.custom ("LogLevel", "min_level", "LogLevelWarning"))
    }

let RollingInterval =
    union {
        kind "Daily"
        kind "Hourly"
    }

let FileSinkArgs =
    combo {
        var (M.custom ("LogLevel", "min_level", "LogLevelInformation"))
        var (M.string ("path"))
        option (M.custom (<@ RollingInterval @>, "rolling"))
    }

let LoggingArgs =
    combo {
        option (M.custom (<@ ConsoleSinkArgs @>, "console"))
        option (M.custom (<@ FileSinkArgs @>, "file"))
    }

let TickerArgs =
    combo {
        var (M.float ("frame_rate", 10.0))
        var (M.bool ("auto_start", true))
    }

let compile segments =
    [
        G.File (segments, ["_Gen"; "Args.fs"],
            G.AutoOpenModule ("Dap.Platform.Args",
                [
                    G.LooseJsonRecord (<@ ConsoleSinkArgs @>, [])
                    G.JsonUnion (<@ RollingInterval @>)
                    G.LooseJsonRecord (<@ FileSinkArgs @>, [])
                    G.LooseJsonRecord (<@ LoggingArgs @>, [])
                    G.LooseJsonRecord (<@ TickerArgs @>, [])
                ]
            )
        )
        G.File (segments, ["_Gen"; "ArgsBuilder.fs"],
            G.BuilderModule ("Dap.Platform.ArgsBuilder",
                [
                    G.ValueBuilder <@ ConsoleSinkArgs @>
                    G.ValueBuilder <@ FileSinkArgs @>
                    G.ValueBuilder <@ LoggingArgs @>
                    G.ValueBuilder <@ TickerArgs @>
                ]
            )
        )
    ]


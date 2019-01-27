module Dap.Archive.Dsl

open Dap.Prelude
open Dap.Context
open Dap.Context.Meta
open Dap.Context.Generator
open Dap.Platform
open Dap.Platform.Meta
open Dap.Platform.Meta.Net
open Dap.Platform.Generator
module TickerTypes = Dap.Platform.Ticker.Types

let RecorderArgs =
    combo {
        var (M.duration ("flush_interval", Duration.FromSeconds 30L))
    }

let compile segments =
    [
        G.File (segments, ["_Gen"; "Args.fs"],
            G.AutoOpenModule ("Dap.Archive.Args",
                [
                    G.PlatformOpens
                    G.LooseJsonRecord (<@ RecorderArgs @>)
                ]
            )
        )
        G.File (segments, ["_Gen"; "ArgsBuilder.fs"],
            G.BuilderModule ("Dap.Archive.ArgsBuilder",
                [
                    G.PlatformBuilderOpens
                    G.ValueBuilder <@ RecorderArgs @>
                ]
            )
        )
    ]


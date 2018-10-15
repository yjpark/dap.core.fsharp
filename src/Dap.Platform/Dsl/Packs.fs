module Dap.Platform.Dsl.Packs

open Dap.Prelude
open Dap.Context
open Dap.Context.Meta
open Dap.Context.Generator
open Dap.Platform
open Dap.Platform.Meta
open Dap.Platform.Generator

let ITickingPack =
    pack [] {
        add (M.tickerService ())
    }

let compile segments =
    [
        G.File (segments, ["_Gen" ; "Packs.fs"],
            G.AutoOpenModule ("Dap.Platform.Packs",
                [
                    G.PackOpens
                    G.PackInterface <@ ITickingPack @>
                ]
            )
        )
    ]

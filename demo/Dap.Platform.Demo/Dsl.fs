module Dap.Platform.Demo.Dsl

open Dap.Prelude
open Dap.Context
open Dap.Context.Builder
open Dap.Context.Generator

let publisher =
    combo {
        string "name" "John Doe" None
        int "year" 2000 None
    }

let compile segments =
    [
        G.File (segments, ["_Gen"; "Types.fs"],
            G.Module ("Dap.Platform.Demo.Types",
                [
                    G.LooseJsonRecord ("Publisher", publisher)
                    G.FinalClass ("PublisherProperty", publisher)
                ]
            )
        )
        G.File (segments, ["_Gen"; "Builder.fs"],
            G.BuilderModule ("Dap.Platform.Demo.Builder",
                [
                    [
                        "open Dap.Platform.Demo.Types"
                    ]
                    G.Builder("publisher", "PublisherProperty", publisher)
                ]
            )
        )
    ]

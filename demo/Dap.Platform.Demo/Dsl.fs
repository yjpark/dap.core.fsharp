module Dap.Platform.Demo.Dsl

open Dap.Prelude
open Dap.Context
open Dap.Context.Builder
open Dap.Context.Generator

let publisher =
    combo {
        string "name" "John Doe"
        int "year" 2000
    }
let IPublisher = Interface.CreateValue "IPublisher" publisher

let person = combo {
    string "name" "John Doe"
    int "age" 30
}

let IPerson = Interface.CreateCombo "IPerson" person

let author = extend person {
    string "publisher" "No Publisher"
}

let compile segments =
    [
        G.File (segments, ["_Gen"; "Types.fs"],
            G.Module ("Dap.Platform.Demo.Types",
                [
                    G.Interface (IPublisher)
                    G.Interface (IPerson)
                    G.LooseJsonRecord ("Publisher", [IPublisher], publisher)
                    G.FinalClass ("Author", [IPerson], author)
                ]
            )
        )
        G.File (segments, ["_Gen"; "Builder.fs"],
            G.BuilderModule ("Dap.Platform.Demo.Builder",
                [
                    "open Dap.Platform.Demo.Types"
                ], [
                    G.Builder("author", "Author", author)
                ]
            )
        )
    ]

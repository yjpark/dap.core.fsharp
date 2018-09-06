module Dap.Platform.Demo.Dsl

open Dap.Prelude
open Dap.Context
open Dap.Context.Builder
open Dap.Context.Generator

let Publisher =
    combo {
        string "name"
        int "year"
    }
let IPublisher = Interface.CreateValue "IPublisher" Publisher

let Person = combo {
    string "name"
    int "age"
}

let IPerson = Interface.CreateCombo "IPerson" Person

let Author = extend Person {
    string "publisher"
}

let Status =
    union {
        kind "Unknown"
        case "Written" (fields {
            string "author"
        })
        case "Published" (fields {
            string "publisher"
            int "year"
        })
    }

let compile segments =
    [
        G.File (segments, ["_Gen"; "Types.fs"],
            G.Module ("Dap.Platform.Demo.Types",
                [
                    G.Interface (IPublisher)
                    G.Interface (IPerson)
                    G.LooseJsonRecord ("Publisher", [IPublisher], Publisher)
                    G.FinalClass ("Author", [IPerson], Author)
                    G.JsonUnion ("Status", Status)
                ]
            )
        )
        G.File (segments, ["_Gen"; "Builder.fs"],
            G.BuilderModule ("Dap.Platform.Demo.Builder",
                [
                    "open Dap.Platform.Demo.Types"
                ], [
                    G.Builder("author", "Author", Author)
                ]
            )
        )
    ]

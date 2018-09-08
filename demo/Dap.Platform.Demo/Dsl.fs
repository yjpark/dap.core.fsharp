module Dap.Platform.Demo.Dsl

open Dap.Prelude
open Dap.Context
open Dap.Context.Meta
open Dap.Context.Generator

let Publisher =
    combo {
        var (M.string "name")
        var (M.int "year")
    }

let IPublisher = Interface.CreateValue "IPublisher" Publisher

let Person = combo {
    var (M.string "name")
    var (M.int "age")
}

let IPerson = Interface.CreateCombo "IPerson" Person

let Author = extend [ <@ Person @> ] {
    var (M.string "publisher")
}

let Status =
    union {
        kind "Unknown"
        case "Written" (fields {
            var (M.string "author")
        })
        case "Published" (fields {
            var (M.string "publisher")
            var (M.int "year")
            option (M.int "copies")
        })
    }

let compile segments =
    [
        G.File (segments, ["_Gen"; "Types.fs"],
            G.Module ("Dap.Platform.Demo.Types",
                [
                    G.Interface IPublisher
                    G.Interface IPerson
                    G.LooseJsonRecord (<@ Publisher @>, [IPublisher])
                    G.FinalClass (<@ Author @>, [IPerson])
                    G.JsonUnion <@ Status @>
                ]
            )
        )
        G.File (segments, ["_Gen"; "Builder.fs"],
            G.BuilderModule ("Dap.Platform.Demo.Builder",
                [
                    "open Dap.Platform.Demo.Types"
                ], [
                    G.Builder <@ Author @>
                ]
            )
        )
    ]

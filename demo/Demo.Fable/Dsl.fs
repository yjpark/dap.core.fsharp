module Demo.Dsl

open Dap.Prelude
open Dap.Context
open Dap.Context.Meta
open Dap.Context.Generator
open Dap.Platform
open Dap.Platform.Meta
open Dap.Platform.Generator
open Dap.Remote.Meta

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

let IClientPack =
    pack [] {
        add (M.proxyService (
                [("UserHubTypes", "Demo.UserHub.Types")],
                "UserHubTypes.Req, UserHubTypes.ClientRes, UserHubTypes.Evt", "UserHubTypes.StubSpec",
                "(getWebSocketUri \"ws_user\")", Some 5.0<second>, true,
                "UserStub", NoKey
            ))
    }

let IAppPack =
    pack [ <@ IClientPack @> ] {
        nothing ()
    }

let App =
    live {
        has <@ IAppPack @>
    }

let compile segments =
    [
        G.File (segments, ["_Gen"; "Types.fs"],
            G.AutoOpenModule ("Demo.Types",
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
            G.BuilderModule ("Demo.Builder",
                [
                    G.ComboBuilder <@ Author @>
                ]
            )
        )
        G.File (segments, ["_Gen"; "App.fs"],
            G.AutoOpenModule ("Demo.App",
                [
                    G.AppOpens
                    G.PackInterface <@ IClientPack @>
                    G.PackInterface <@ IAppPack @>
                    G.AppInterface <@ App @>
                    G.AppClass <@ App @>
                ]
            )
        )
    ]
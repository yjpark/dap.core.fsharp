module Demo.Dsl

open Dap.Prelude
open Dap.Context
open Dap.Context.Meta
open Dap.Context.Generator
open Dap.Platform
open Dap.Platform.Meta
open Dap.Platform.Generator
open Dap.Remote.Meta

let IPublisher =
    combo {
        var (M.string "name")
        var (M.int "year")
    }

let Publisher =
    extend [ <@ IPublisher @> ] {
        nothing ()
    }

let IPerson = combo {
    var (M.string "name")
    var (M.int "age")
}

let Author = extend [ <@ IPerson @> ] {
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
        add (M.proxy (
                [("UserHubTypes", "Demo.UserHub.Types")],
                "UserHubTypes.Req, UserHubTypes.ClientRes, UserHubTypes.Evt", "UserHubTypes.StubSpec",
                uri = "(getWebSocketUri \"ws_user\")",
                retryDelay = 5.0<second>,
                kind = "UserStub"
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
                    G.ValueInterface <@ IPublisher @>
                    G.ComboInterface <@ IPerson @>
                    G.LooseJsonRecord (<@ Publisher @>)
                    G.FinalCombo (<@ Author @>)
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
                    G.App <@ App @>
                ]
            )
        )
    ]

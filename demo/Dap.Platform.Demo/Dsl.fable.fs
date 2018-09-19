module Dap.Platform.Demo.Dsl

open Dap.Prelude
open Dap.Context
open Dap.Context.Meta
open Dap.Context.Generator
open Dap.Platform
open Dap.Platform.Meta
open Dap.Platform.Generator

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

let IServicesPack =
    pack [] {
        extra (M.codeArgs ([], "int", "2", "service"))
        add (M.service ([], M.noArgs, "FakeService", "FakeService.spec", "Fake"))
    }

let ICommonPack =
    pack [ <@ IServicesPack @> ] {
        extra (M.codeArgs ([], "int", "100", "common"))
    }
let IBackupPack =
    pack [ <@ ICommonPack @> ] {
        extra (M.codeArgs ([], "int", "200", "backup"))
        add (M.service ([], M.noArgs, "FakeService", "FakeService.spec", "Fake", "Another"))
    }

let IAppPack =
    pack [ <@ ICommonPack @> ; <@ IServicesPack @> ] {
        //register (M.spawner ("TestArgs", "TestAgent", "Test", "test"))
        extra (M.codeArgs ([], "int", "100", "test"))
        add_pack <@ IBackupPack @> (M.service ([], M.noArgs, "FakeView", "FakeView.spec", "View"))
    }

let App =
    live {
        has <@ IAppPack @>
        has <@ IBackupPack @>
    }
let compile segments =
    [
        G.File (segments, ["_Gen.Fable"; "Types.fs"],
            G.AutoOpenModule ("Dap.Platform.Demo.Types",
                [
                    G.Interface IPublisher
                    G.Interface IPerson
                    G.LooseJsonRecord (<@ Publisher @>, [IPublisher])
                    G.FinalClass (<@ Author @>, [IPerson])
                    G.JsonUnion <@ Status @>
                ]
            )
        )
        G.File (segments, ["_Gen.Fable"; "Builder.fs"],
            G.BuilderModule ("Dap.Platform.Demo.Builder",
                [
                    [
                        "open Dap.Platform.Demo.Types"
                    ]
                    G.ComboBuilder <@ Author @>
                ]
            )
        )
        G.File (segments, ["_Gen.Fable"; "App.fs"],
            G.AutoOpenModule ("Dap.Platform.Demo.App",
                [
                    G.AppOpens
                    G.PackInterface <@ IServicesPack @>
                    G.PackInterface <@ ICommonPack @>
                    G.PackInterface <@ IAppPack @>
                    G.PackInterface <@ IBackupPack @>
                    G.AppInterface <@ App @>
                    G.AppClass <@ App @>
                ]
            )
        )
    ]
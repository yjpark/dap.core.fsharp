module Demo.Dsl

open Dap.Prelude
open Dap.Context
open Dap.Context.Meta
open Dap.Context.Generator
open Dap.Platform
open Dap.Platform.ArgsBuilder
open Dap.Platform.Meta
open Dap.Platform.Meta.Net
open Dap.Platform.Generator

let IPublisher =
    combo {
        var (M.string "name")
        var (M.int "year")
    }

let Publisher =
    extend [ <@ IPublisher @> ] {
        nothing ()
    }

let IPerson =
    combo {
        var (M.string "name")
        var (M.int "age")
    }

let Author =
    extend [ <@ IPerson @> ] {
        var (M.string "publisher")
        prop (M.combo "books")
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
        add (M.ticker ())
    }

let backupTickerArgs =
    ticker_args {
        frame_rate 1.0
    }

let ICommonPack =
    pack [ <@ IServicesPack @> ] {
        extra (M.codeArgs ("int", "100", "common"))
    }
let IBackupPack =
    pack [ <@ ICommonPack @> ] {
        add (M.ticker (backupTickerArgs, "Backup"))
    }

let IAppPack =
    pack [ <@ ICommonPack @> ; <@ IServicesPack @> ] {
        //register (M.agent ("TestArgs", "TestAgent", "Test", "test"))
        extra (M.codeArgs ("int", "100", "test"))
    }

let App =
    live {
        has <@ IAppPack @>
        has <@ IBackupPack @>
    }

let compile segments =
    [
        G.File (segments, ["_Gen"; "Types.fs"],
            G.AutoOpenModule ("Demo.Types",
                [
                    G.ValueInterface <@ IPublisher @>
                    G.ComboInterface <@ IPerson @>
                    G.LooseJsonRecord <@ Publisher @>
                    G.FinalCombo <@ Author @>
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
                    G.PackInterface <@ IServicesPack @>
                    G.PackInterface <@ ICommonPack @>
                    G.PackInterface <@ IAppPack @>
                    G.PackInterface <@ IBackupPack @>
                    G.App <@ App @>
                ]
            )
        )
    ]

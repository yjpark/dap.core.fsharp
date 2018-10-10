#load @".paket/load/netstandard2.0/Newtonsoft.Json.fsx"
#load @".paket/load/netstandard2.0/Thoth.Json.Net.fsx"
#load @".paket/load/netstandard2.0/NodaTime.fsx"
#r @"src/Dap.Prelude/bin/Release/netstandard2.0/Dap.Prelude.dll"
#r @"src/Dap.Context/bin/Release/netstandard2.0/Dap.Context.dll"
#r @"src/Dap.Platform/bin/Release/netstandard2.0/Dap.Platform.dll"

open Dap.Prelude
#load "src/Fable.Dap.Platform/Shared/Clock.fs"
#load "src/Fable.Dap.Dsl/Platform/MG.fs"
#load "src/Dap.Platform/Dsl/Args.fs"
#load "src/Dap.Platform/Dsl/Stats.fs"
#load "src/Dap.Platform/Dsl/Packs.fs"
#load "src/Dap.Archive/Dsl.fs"

Dap.Platform.Dsl.Args.compile ["src" ; "Dap.Platform"]
|> List.iter ^<| printfn "%s\n"

Dap.Platform.Dsl.Stats.compile ["src" ; "Dap.Platform"]
|> List.iter ^<| printfn "%s\n"

Dap.Platform.Dsl.Packs.compile ["src" ; "Dap.Platform"]
|> List.iter ^<| printfn "%s\n"

Dap.Archive.Dsl.compile ["src" ; "Dap.Archive"]
|> List.iter ^<| printfn "%s\n"

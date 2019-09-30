#load @".paket/load/netstandard2.1/Newtonsoft.Json.fsx"
#load @".paket/load/netstandard2.1/Thoth.Json.Net.fsx"
#load @".paket/load/netstandard2.1/NodaTime.fsx"
#r @"src/Dap.Prelude/bin/Release/netstandard2.1/Dap.Prelude.dll"
#r @"src/Dap.Context/bin/Release/netstandard2.1/Dap.Context.dll"
#r @"src/Dap.Platform/bin/Release/netstandard2.1/Dap.Platform.dll"

open Dap.Prelude
#load "src/Dap.Platform/Dsl/Packs.fs"

Dap.Platform.Dsl.Packs.compile ["src" ; "Dap.Platform"]
|> List.iter ^<| printfn "%s\n"


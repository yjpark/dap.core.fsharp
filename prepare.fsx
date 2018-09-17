#load @".paket/load/netstandard2.0/Newtonsoft.Json.fsx"
#load @".paket/load/netstandard2.0/Thoth.Json.Net.fsx"
#load @".paket/load/netstandard2.0/NodaTime.fsx"
#r @"src/Dap.Prelude/bin/Release/netstandard2.0/Dap.Prelude.dll"
#r @"src/Dap.Context/bin/Release/netstandard2.0/Dap.Context.dll"
#r @"src/Dap.Platform/bin/Release/netstandard2.0/Dap.Platform.dll"

open Dap.Prelude
#load "src/Dap.Platform/Dsl.fs"
#load "src/Dap.Archive/Dsl.fs"

Dap.Platform.Dsl.compile ["src" ; "Dap.Platform"]
|> List.iter ^<| printfn "%s\n"

Dap.Archive.Dsl.compile ["src" ; "Dap.Archive"]
|> List.iter ^<| printfn "%s\n"

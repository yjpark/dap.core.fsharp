#!/usr/bin/env -S dotnet fsi

#load @".paket/load/Newtonsoft.Json.fsx"
#load @".paket/load/Thoth.Json.Net.fsx"
#load @".paket/load/NodaTime.fsx"
#r @"src/Dap.Prelude/bin/Release/net6.0/Dap.Prelude.dll"
#r @"src/Dap.Context/bin/Release/net6.0/Dap.Context.dll"
#r @"src/Dap.Platform/bin/Release/net6.0/Dap.Platform.dll"

open Dap.Prelude
#load "src/Dap.Platform/Dsl/Packs.fs"

Dap.Platform.Dsl.Packs.compile ["src" ; "Dap.Platform"]
|> List.iter ^<| printfn "%s\n"


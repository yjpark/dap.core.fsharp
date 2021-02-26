#!/usr/bin/env -S dotnet fsi

#load @".paket/load/Newtonsoft.Json.fsx"
#load @".paket/load/Thoth.Json.Net.fsx"
#load @".paket/load/NodaTime.fsx"
#r @"src/Dap.Prelude/bin/Release/net5.0/Dap.Prelude.dll"
#r @"src/Dap.Context/bin/Release/net5.0/Dap.Context.dll"
#r @"src/Dap.Platform/bin/Release/net5.0/Dap.Platform.dll"

open Dap.Prelude
#load "src/Dap.WebSocket/Dsl.fs"

Dap.WebSocket.Dsl.compile ["src" ; "Dap.WebSocket"]
|> List.iter ^<| printfn "%s\n"

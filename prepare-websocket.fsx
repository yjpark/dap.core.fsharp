#load @".paket/load/netstandard2.0/Newtonsoft.Json.fsx"
#load @".paket/load/netstandard2.0/Thoth.Json.Net.fsx"
#load @".paket/load/netstandard2.0/NodaTime.fsx"
#r @"src/Dap.Prelude/bin/Release/netstandard2.0/Dap.Prelude.dll"
#r @"src/Dap.Context/bin/Release/netstandard2.0/Dap.Context.dll"
#r @"src/Dap.Platform/bin/Release/netstandard2.0/Dap.Platform.dll"

open Dap.Prelude
#load "src/Dap.WebSocket/Dsl.fs"

Dap.WebSocket.Dsl.compile ["src" ; "Dap.WebSocket"]
|> List.iter ^<| printfn "%s\n"
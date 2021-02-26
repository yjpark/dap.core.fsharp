#!/usr/bin/env -S dotnet fsi

#load @".paket/load/Thoth.Json.Net.fsx"
#r @"src/Dap.Prelude/bin/Release/net5.0/Dap.Prelude.dll"
#r @"src/Dap.Context/bin/Release/net5.0/Dap.Context.dll"
#r @"src/Dap.Platform/bin/Release/net5.0/Dap.Platform.dll"
#r @"src/Dap.WebSocket/bin/Release/net5.0/Dap.WebSocket.dll"
#r @"src/Dap.Remote/bin/Release/net5.0/Dap.Remote.dll"

open Dap.Prelude
#load "demo/Demo.Fable/src/Dsl.fs"

Demo.Dsl.compile ["demo" ; "Demo.Fable"]
|> List.iter ^<| printfn "%s\n"

#load @".paket/load/netstandard2.0/Fable.Core.fsx"
#load @".paket/load/netstandard2.0/Fable.Import.Browser.fsx"
#load @".paket/load/netstandard2.0/Fable.PowerPack.fsx"
#load @".paket/load/netstandard2.0/Thoth.Json.fsx"
#r @"src/Dap.Prelude/bin/Release/netstandard2.0/Dap.Prelude.dll"
#r @"src/Dap.Context/bin/Release/netstandard2.0/Dap.Context.dll"
#r @"src/Dap.Platform/bin/Release/netstandard2.0/Dap.Platform.dll"
#r @"src/Fable.Dap.WebSocket/bin/Release/netstandard2.0/Dap.WebSocket.dll"
#r @"src/Fable.Dap.Remote/bin/Release/netstandard2.0/Dap.Remote.dll"

open Dap.Prelude
#load "demo/Demo.Fable/Dsl.fs"

Demo.Dsl.compile ["demo" ; "Demo.Fable"]
|> List.iter ^<| printfn "%s\n"

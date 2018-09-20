#load @".paket/load/netstandard2.0/Fable.Core.fsx"
#load @".paket/load/netstandard2.0/Fable.Import.Browser.fsx"
#load @".paket/load/netstandard2.0/Fable.PowerPack.fsx"
#load @".paket/load/netstandard2.0/Thoth.Json.fsx"
#r @"src/Fable.Dap.Prelude/bin/Release/netstandard2.0/Fable.Dap.Prelude.dll"
#r @"src/Fable.Dap.Context/bin/Release/netstandard2.0/Fable.Dap.Context.dll"
#r @"src/Fable.Dap.Platform/bin/Release/netstandard2.0/Fable.Dap.Platform.dll"
#r @"src/Fable.Dap.WebSocket/bin/Release/netstandard2.0/Fable.Dap.WebSocket.dll"
#r @"src/Fable.Dap.Remote/bin/Release/netstandard2.0/Fable.Dap.Remote.dll"
#r @"src/Fable.Dap.Dsl/bin/Release/netstandard2.0/Fable.Dap.Dsl.dll"

open Dap.Prelude
#load "demo/Demo.Fable/Dsl.fs"

Demo.Dsl.compile ["demo" ; "Demo.Fable"]
|> List.iter ^<| printfn "%s\n"

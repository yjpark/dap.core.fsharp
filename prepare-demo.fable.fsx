#load @".paket/load/netstandard2.0/Fable.Core.fsx"
#load @".paket/load/netstandard2.0/Fable.Import.Browser.fsx"
#load @".paket/load/netstandard2.0/Fable.PowerPack.fsx"
#load @".paket/load/netstandard2.0/Thoth.Json.fsx"
#r @"src/Fable.Dap.Prelude/bin/Release/netstandard2.0/Fable.Dap.Prelude.dll"
#r @"src/Fable.Dap.Context/bin/Release/netstandard2.0/Fable.Dap.Context.dll"
#r @"src/Fable.Dap.Platform/bin/Release/netstandard2.0/Fable.Dap.Platform.dll"

open Dap.Prelude
#load "demo/Dap.Platform.Demo/Dsl.fable.fs"

Dap.Platform.Demo.Dsl.compile ["demo" ; "Dap.Platform.Demo"]
|> List.iter ^<| printfn "%s\n"

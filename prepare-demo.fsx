#load @".paket/load/netstandard2.0/Newtonsoft.Json.fsx"
#load @".paket/load/netstandard2.0/Thoth.Json.Net.fsx"
#r @"src/Dap.Prelude/bin/Release/netstandard2.0/Dap.Prelude.dll"
#r @"src/Dap.Context/bin/Release/netstandard2.0/Dap.Context.dll"
#r @"src/Dap.Platform/bin/Release/netstandard2.0/Dap.Platform.dll"

open Dap.Prelude
#load "demo/Dap.Platform.Demo/Dsl.fs"

Dap.Platform.Demo.Dsl.compile ["demo" ; "Dap.Platform.Demo"]
|> List.iter ^<| printfn "%s\n"

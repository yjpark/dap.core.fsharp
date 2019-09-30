#load @".paket/load/netstandard2.1/Newtonsoft.Json.fsx"
#load @".paket/load/netstandard2.1/Thoth.Json.Net.fsx"
#r @"src/Dap.Prelude/bin/Release/netstandard2.1/Dap.Prelude.dll"
#r @"src/Dap.Context/bin/Release/netstandard2.1/Dap.Context.dll"
#r @"src/Dap.Platform/bin/Release/netstandard2.1/Dap.Platform.dll"

open Dap.Prelude
#load "demo/Demo/Dsl.fs"

Demo.Dsl.compile ["demo" ; "Demo"]
|> List.iter ^<| printfn "%s\n"

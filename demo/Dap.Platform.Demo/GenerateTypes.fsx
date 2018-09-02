#load @"../../.paket/load/netstandard2.0/NewtonSoft.Json.fsx"
#load @"../../.paket/load/netstandard2.0/Thoth.Json.Net.fsx"
#r @"../../src/Dap.Prelude/bin/Release/netstandard2.0/Dap.Prelude.dll"
#r @"../../src/Dap.Context/bin/Release/netstandard2.0/Dap.Context.dll"

open Dap.Prelude
#load "Dsl.fs"

Dap.Platform.Demo.Dsl.compile []
|> List.iter ^<| printfn "%s\n"
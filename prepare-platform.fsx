#load @".paket/load/netstandard2.1/Newtonsoft.Json.fsx"
#load @".paket/load/netstandard2.1/Thoth.Json.Net.fsx"
#load @".paket/load/netstandard2.1/NodaTime.fsx"
#r @"src/Dap.Prelude/bin/Release/netstandard2.1/Dap.Prelude.dll"
#r @"src/Dap.Context/bin/Release/netstandard2.1/Dap.Context.dll"

open Dap.Prelude
#load "src/Fable.Dap.Platform/Shared/Logic.fs"
#load "src/Fable.Dap.Platform/Shared/Types.fs"
#load "src/Fable.Dap.Platform/Shared/Clock.fs"
#load "src/Fable.Dap.Platform/Shared/EDS.fs"
#load "src/Dap.Platform/Meta/Extensions.fs"
#load "src/Dap.Platform/Meta/Net/Extensions.fs"
#load "src/Dap.Platform/Dsl/Args.fs"
#load "src/Dap.Platform/Dsl/Dash.fs"

Dap.Platform.Dsl.Args.compile ["src" ; "Dap.Platform"]
|> List.iter ^<| printfn "%s\n"

Dap.Platform.Dsl.Dash.compile ["src" ; "Dap.Platform"]
|> List.iter ^<| printfn "%s\n"


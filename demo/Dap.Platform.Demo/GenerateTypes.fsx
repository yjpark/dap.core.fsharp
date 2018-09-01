#load @"../../.paket/load/netstandard2.0/NewtonSoft.Json.fsx"
#load @"../../.paket/load/netstandard2.0/Thoth.Json.Net.fsx"
#r @"../../src/Dap.Prelude/bin/Release/netstandard2.0/Dap.Prelude.dll"
#r @"../../src/Dap.Context/bin/Release/netstandard2.0/Dap.Context.dll"

open Dap.Prelude
open Dap.Context
open Dap.Context.Builder
open Dap.Context.Generator

let publisher =
    combo {
        string "name" "John Doe" None
        int "year" 2000 None
    }

G.File ("_Gen", "Types",
    G.Module ("Dap.Platform.Demo.Types",
        [
            G.LooseJsonRecord ("Publisher", publisher)
            G.FinalClass ("PublisherProperty", publisher)
        ]
    )
)

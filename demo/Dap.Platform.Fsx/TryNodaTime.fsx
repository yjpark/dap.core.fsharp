#load @"../../.paket/load/netstandard2.0/Serilog.fsx"
#load @"../../.paket/load/netstandard2.0/Serilog.Sinks.Console.fsx"
#load @"../../.paket/load/netstandard2.0/Serilog.Sinks.File.fsx"
#load @"../../.paket/load/netstandard2.0/Serilog.Formatting.Compact.fsx"
#load @"../../.paket/load/netstandard2.0/NodaTime.fsx"
#load @"../../.paket/load/netstandard2.0/Elmish.fsx"
#r @"../../src/Dap.Prelude/bin/Release/netstandard2.0/Dap.Prelude.dll"
#r @"../../src/Dap.Context/bin/Release/netstandard2.0/Dap.Context.dll"
#r @"../../src/Dap.Platform/bin/Release/netstandard2.0/Dap.Platform.dll"

open NodaTime

open Dap.Prelude
open Dap.Context
open Dap.Platform

fsi.ShowDeclarationValues <- false

let logging = Logging.setupConsole None
let env = Env.live MailboxPlatform logging "NodaTime"

logWarn logging "AAAAAA" "TEST" env

fsi.ShowDeclarationValues <- true

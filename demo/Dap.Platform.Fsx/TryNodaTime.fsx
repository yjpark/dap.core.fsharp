#load @"../../.paket/load/netstandard2.0/Serilog.fsx"
#load @"../../.paket/load/netstandard2.0/Serilog.Sinks.Console.fsx"
#load @"../../.paket/load/netstandard2.0/Serilog.Sinks.File.fsx"
#load @"../../.paket/load/netstandard2.0/Serilog.Formatting.Compact.fsx"
#load @"../../.paket/load/netstandard2.0/NodaTime.fsx"
#load @"../../.paket/load/netstandard2.0/Elmish.fsx"
#r @"../../src/Dap.Platform.Net/bin/Debug/netstandard2.0/Dap.Platform.dll"
#r @"../../src/Dap.Platform.Net/bin/Debug/netstandard2.0/Dap.Platform.Net.dll"

open NodaTime
open Dap.Platform

fsi.ShowDeclarationValues <- false

let logging = Logging.setupConsole None
let env = Env.live MailboxPlatform logging "NodaTime"

fsi.ShowDeclarationValues <- true

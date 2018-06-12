
#load @"../../.paket/load/Logary.fsx"
#load @"../../.paket/load/Logary.fsx"
#load @"../../.paket/load/Elmish.fsx"
#r @"../../src/Dap.Platform.Net/bin/Debug/netstandard2.0/Dap.Platform.dll"
#r @"../../src/Dap.Platform.Net/bin/Debug/netstandard2.0/Dap.Platform.Net.dll"

open Logary
open Dap.Platform

fsi.ShowDeclarationValues <- false

let logging = setupConsole None
let env = Env.live MailboxPlatform logging "Test"

env
|> Env.post (DoGetAgent ("test", "test", None))
|> Env.post (DoGetAgent ("test", "test2", None))

fsi.ShowDeclarationValues <- false

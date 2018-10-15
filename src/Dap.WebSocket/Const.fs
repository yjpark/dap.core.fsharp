[<AutoOpen>]
module Dap.WebSocket.Const

open Dap.Platform

[<Literal>]
let DefaultBufferSize : int = 1048576

let DefaultRefreshInterval = Duration.FromSeconds 5L

let DefaultPktSlowCap = Duration.FromMilliseconds 500L

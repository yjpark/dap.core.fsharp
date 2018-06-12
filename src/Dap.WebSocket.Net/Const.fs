[<AutoOpen>]
module Dap.WebSocket.Const

open Dap.Platform

[<Literal>]
let DefaultBufferSize : int = 1048576

[<Literal>]
let DefaultWebSocketReplySlowCap : float<ms> = 2000.0<ms>

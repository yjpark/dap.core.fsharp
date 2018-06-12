[<AutoOpen>]
module Dap.Platform.Const

open Dap.Prelude

[<Literal>]
let DefaultDeliverSlowCap : float<ms> = 100.0<ms>

[<Literal>]
let DefaultProcessSlowCap : float<ms> = 100.0<ms>

[<Literal>]
let DefaultReplySlowCap : float<ms> = 100.0<ms>

[<Literal>]
let DefaultFuncSlowCap : float<ms> = 100.0<ms>

[<Literal>]
let DefaultTaskSlowCap : float<ms> = 60000.0<ms>

let ``AckLogLevel`` = LogLevelDebug

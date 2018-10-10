[<AutoOpen>]
module Dap.Platform.Const

open Dap.Prelude

[<Literal>]
let EnvConsoleKind = "EnvConsole"
let AgentConsoleKind = "AgentConsole"

let DefaultSlowCap = NodaTime.Duration.FromMilliseconds 200L
let DefaultTaskSlowCap = NodaTime.Duration.FromMilliseconds 500L
let DefaultLongTaskSlowCap = NodaTime.Duration.FromSeconds 10L

let ``AckLogLevel`` = LogLevelDebug

[<AutoOpen>]
[<RequireQualifiedAccess>]
module Dap.Platform.Env

open Dap.Prelude

type Env (scope', logging') =
    let scope : Scope = scope'
    let logging : ILogging = logging'
    interface ILogger with
        member _this.Log m = logging.Log m
    interface IEnv with
        member _this.Logging = logging
        member _this.Scope = scope

let spawn spec kind key (env : IEnv) : IAgent =
    (Agent.spawn spec <| AgentParam.Create env kind key)
    :> IAgent


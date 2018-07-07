[<AutoOpen>]
[<RequireQualifiedAccess>]
module Dap.Platform.Env

open Dap.Prelude

type Env (scope', logging') =
    let scope : Scope = scope'
    let logging : ILogging = logging'
    //ILogger
    member _this.Log m = logging.Log m
    interface ILogger with
        member this.Log m = this.Log m
    //IEnv
    member this.Logging = logging
    member this.Scope = scope
    interface IEnv with
        member _this.Logging = logging
        member _this.Scope = scope
    //IOwner
    interface IOwner with
        member _this.Ident = scope
        member _this.Disposed = false

let spawn spec kind key (env : IEnv) : IAgent =
    (Agent.spawn spec <| AgentParam.Create env kind key)
    :> IAgent


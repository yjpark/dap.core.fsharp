[<AutoOpen>]
[<RequireQualifiedAccess>]
module Dap.Platform.Env

open Dap.Prelude

type internal Env (logging', scope', clock') =
    let logging : ILogging = logging'
    let scope : Scope = scope'
    let clock : IClock = clock'
    //ILogger
    member __.Log m = logging.Log m
    interface ILogger with
        member this.Log m = this.Log m
    //IRunner
    member __.Clock = clock
    interface IRunner with
        member this.Clock = clock
    //IEnv
    member this.Logging = logging
    member this.Scope = scope
    interface IEnv with
        member __.Logging = logging
        member __.Scope = scope
    //IOwner
    interface IOwner with
        member __.Luid = scope
        member __.Disposed = false

let spawn spec kind key (env : IEnv) : IAgent =
    (Agent.spawn spec <| AgentParam.Create env kind key)
    :> IAgent

let live logging scope =
    new Env (logging, scope, RealClock()) :> IEnv


[<AutoOpen>]
module Dap.Platform.Types'

open Dap.Prelude
open Dap.Context

type IEnv =
    inherit IRunner
    inherit IOwner
    abstract Logging : ILogging with get
    abstract Scope : Scope with get

type AgentParam = {
    Env : IEnv
    Kind : Kind
    Key : Key
} with
    static member Create env kind key =
        {
            Env = env
            Kind = kind
            Key = key
        }

type IAgent =
    inherit IOwner
    inherit IRunner
    abstract Env : IEnv with get
    abstract Ident : Ident with get

and IAgent<'req, 'evt> when 'req :> IReq and 'evt :> IEvt =
    inherit IAgent
    inherit IPoster<'req>
    abstract Actor2 : IActor<'req, 'evt> with get
    abstract AsAgent1 : IAgent with get

and IAgent<'msg> when 'msg :> IMsg =
    inherit IAgent
    abstract Deliver : 'msg -> unit

and IAgent<'args, 'model, 'msg, 'req, 'evt> when 'msg :> IMsg and 'req :> IReq and 'evt :> IEvt =
    inherit IAgent<'req, 'evt>
    inherit IAgent<'msg>
    abstract Actor : IActor<'args, 'model, 'req, 'evt> with get
    abstract AsAgent2 : IAgent<'req, 'evt> with get
    abstract AsAgent2' : IAgent<'msg> with get

type AgentEvt = NoEvt

type IPack =
    inherit IRunner
    abstract Env : IEnv with get

type IApp<'app when 'app :> IPack> =
    inherit IRunner<'app>
    inherit IPack
    abstract Setup : unit -> unit

and IPackAgent<'pack when 'pack :> IPack> =
    inherit IAgent
    abstract Pack : 'pack with get
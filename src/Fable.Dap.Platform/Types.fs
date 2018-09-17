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
    abstract AsAgent1 : IAgent with get

and IAgent<'req, 'evt> when 'req :> IReq and 'evt :> IEvt =
    inherit IAgent
    inherit IPoster<'req>
    abstract Actor : IActor<'req, 'evt> with get
    abstract AsAgent2 : IAgent<'req, 'evt> with get

and IAgent<'msg> when 'msg :> IMsg =
    inherit IAgent
    abstract Deliver : 'msg -> unit
    abstract AsAgent2' : IAgent<'msg> with get

and IAgent<'args, 'model, 'msg, 'req, 'evt> when 'msg :> IMsg and 'req :> IReq and 'evt :> IEvt =
    inherit IAgent<'req, 'evt>
    inherit IAgent<'msg>
    abstract Actor : IActor<'args, 'model, 'req, 'evt> with get
    abstract AsAgent3 : IAgent<'args, 'model, 'msg, 'req, 'evt> with get

and IPackAgent<'pack> =
    inherit IAgent
    abstract Pack : 'pack with get

type AgentEvt = NoEvt
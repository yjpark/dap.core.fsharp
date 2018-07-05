[<AutoOpen>]
module Dap.Platform.Types'

open Dap.Prelude

type IRunner =
    inherit ILogger

type IAgent =
    inherit IOwner
    inherit IRunner
    abstract Ident : Ident with get

and IAgent<'req, 'evt> when 'req :> IReq and 'evt :> IEvt =
    inherit IAgent
    inherit IPoster<'req>
    abstract Actor : IActor<'req, 'evt> with get

and IAgent<'msg> when 'msg :> IMsg =
    inherit IAgent
    abstract Deliver : 'msg -> unit

and IAgent<'args, 'model, 'msg, 'req, 'evt> when 'msg :> IMsg and 'req :> IReq and 'evt :> IEvt =
    inherit IAgent<'req, 'evt>
    inherit IAgent<'msg>
    abstract Actor : IActor<'args, 'model, 'req, 'evt> with get

type AgentEvt = NoEvt
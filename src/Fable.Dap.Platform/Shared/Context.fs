[<RequireQualifiedAccess>]
module Dap.Platform.Context

open Dap.Prelude
open Dap.Context

type Args<'context when 'context :> IContext> = IAgent -> 'context

and Req = NoReq

and Evt = NoEvt

and Msg = NoMsg

and Agent<'context when 'context : not struct and 'context :> IContext> (param) =
    inherit BaseAgent<Agent<'context>, Args<'context>, 'context, Msg, Req, Evt> (param)
    override this.Runner = this
    static member Spawn (param) = new Agent<'context> (param)
    member this.Context = this.Actor.State

let private init : ActorInit<Args<'context>, 'context, Msg> =
    fun runner args ->
        (args (runner :> IAgent), noCmd)

let private update : Update<Agent<'context>, 'context, Msg> =
    fun _runner _msg model ->
        (model, noCmd)

let spec<'context when 'context : not struct and 'context :> IContext> (args) =
    new ActorSpec<Agent<'context>, Args<'context>, 'context, Msg, Req, Evt>
        (Agent<'context>.Spawn, args, noWrapReq, noCastEvt, init, update)
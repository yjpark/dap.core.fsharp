[<RequireQualifiedAccess>]
module Dap.Platform.State

#if FABLE_COMPILER
open Fable.Core
#endif

open Dap.Prelude
open Dap.Context

type Args<'state> = IAgent -> 'state

and Req = NoReq

and Evt = NoEvt

and Msg = NoMsg

and Agent<'state when 'state : not struct> (param) =
    inherit BaseAgent<Agent<'state>, Args<'state>, 'state, Msg, Req, Evt> (param)
    override this.Runner = this
    static member Spawn (param) = new Agent<'state> (param)
    member this.State = this.Actor.State

let private init : ActorInit<Args<'state>, 'state, Msg> =
    fun runner args ->
        (args (runner :> IAgent), noCmd)

let private update : Update<Agent<'state>, 'state, Msg> =
    fun _runner _msg model ->
        (model, noCmd)

#if FABLE_COMPILER
[<PassGenericsAttribute>]
#endif
let spec<'state when 'state : not struct> (args) =
    new ActorSpec<Agent<'state>, Args<'state>, 'state, Msg, Req, Evt>
        (Agent<'state>.Spawn, args, noWrapReq, noCastEvt, init, update)
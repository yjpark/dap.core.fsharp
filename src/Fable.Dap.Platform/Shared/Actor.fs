[<AutoOpen>]
module Dap.Platform.Actor

#if !FABLE_COMPILER
open System.Threading.Tasks
#endif

open Dap.Prelude

//Note: during Init, the model is not created yet
type ActorInit<'args, 'model, 'msg> when 'msg :> IMsg =
    Init<IAgent<'msg>, 'args, 'model, 'msg>

/// Change model according to msg, also may generate cmds.
type ActorUpdate<'args, 'model, 'msg, 'req, 'evt> when 'msg :> IMsg and 'req :> IReq and 'evt :> IEvt =
    Update<IAgent<'args, 'model, 'msg, 'req, 'evt>, 'model, 'msg>

/// Generate msg from outside. e.g. an timer, or keyboard.
type ActorSubscribe<'args, 'model, 'msg, 'req, 'evt> when 'msg :> IMsg and 'req :> IReq and 'evt :> IEvt =
    Subscribe<IAgent<'args, 'model, 'msg, 'req, 'evt>, 'model, 'msg>

type ActorSpec<'args, 'model, 'msg, 'req, 'evt> when 'msg :> IMsg and 'req :> IReq and 'evt :> IEvt =
    ActorSpec'<IAgent<'msg>, IAgent<'args, 'model, 'msg, 'req, 'evt>, 'args, 'model, 'msg, 'req, 'evt>

type ActorOperate<'args, 'model, 'msg, 'req, 'evt> when 'msg :> IMsg and 'req :> IReq and 'evt :> IEvt =
    Operate<IAgent<'args, 'model, 'msg, 'req, 'evt>, 'model, 'msg>

[<StructuredFormatDisplay("<Actor>{AsDisplay}")>]
type internal Actor<'args, 'model, 'msg, 'req, 'evt when 'model : not struct and 'msg :> IMsg and 'req :> IReq and 'evt :> IEvt> (agent', spec', args', model') =
    let agent : IAgent<'msg> = agent'
    let spec : IActorSpec<'msg, 'req, 'evt> = spec'
    let args : 'args = args'
    let bus = new Bus<'evt> (agent :> IOwner)
    let mutable state : 'model = model'
    let mutable version : Version = Version.Init
    let tryFireEvent (msg : 'msg) =
        match spec.CastEvt msg with
        | None ->
            ()
        | Some evt ->
            version <- version.IncEvt
            bus.Trigger evt
    member _this.AsDisplay = (agent.Ident, version)
    member _this.State = state
    member _this.SetState (msg : 'msg) (newState : 'model) =
        state <- newState
        version <- version.IncMsg (not (newState =? state))
        tryFireEvent msg
    member _this.Handle (req : 'req) =
        version <- version.IncReq
        agent.Deliver (spec.WrapReq req)
#if !FABLE_COMPILER
    member _this.HandleAsync (getReq : Callback<'res> -> 'req) =
        version <- version.IncReq
        agent.DeliverAsync (spec.WrapReq << getReq)
#endif
    member _this.Version = version
    interface IActor<'args, 'model, 'req, 'evt> with
        member this.Handle req = this.Handle req
#if !FABLE_COMPILER
        member this.HandleAsync getReq = this.HandleAsync getReq
#endif
        member _this.OnEvent = bus.Publish
        member _this.Ident = agent.Ident
        member _this.Args = args
        member _this.State = state
        member _this.Version = version


[<AutoOpen>]
module Dap.Platform.Actor

#if !FABLE_COMPILER
open System.Threading.Tasks
#endif

open Dap.Prelude
open Dap.Context
open Dap.Platform

//Note: during Init, the model is not created yet
type ActorInit<'args, 'model, 'msg>
            when 'model : not struct and 'msg :> IMsg =
    Init<IAgent<'msg>, 'args, 'model, 'msg>

type ActorParam<'msg> when 'msg :> IMsg = {
    WrapEvt : Wrapper<'msg, AgentEvt> option
} with
    member this.WithWrapEvt v = {this with WrapEvt = Some v}


let noActorParam<'msg when 'msg :> IMsg> : ActorParam<'msg> =
    {
        WrapEvt = None
    }

type Spawner<'runner> =
    AgentParam -> 'runner

type ActorSpec<'runner, 'args, 'model, 'msg, 'req, 'evt
            when 'runner :> IAgent<'args, 'model, 'msg, 'req, 'evt>
                and 'model : not struct and 'msg :> IMsg
                and 'req :> IReq and 'evt :> IEvt>
        (spawner', args', wrapReq', castEvt', init', update') =
    let spawner : Spawner<'runner> = spawner'
    let args : 'args = args'
    let wrapReq : Wrapper<'msg, 'req> = wrapReq'
    let castEvt : CastEvt<'msg, 'evt> = castEvt'
    let mutable inUse : bool = false
    let mutable logic : Logic<IAgent<'msg>, 'runner, 'args, 'model, 'msg> =
        let noSubscription : Subscribe<'runner, 'model, 'msg> =
            fun _runner _model -> noCmd
        {
            Init = init'
            Update = update'
            Subscribe = noSubscription
        }
    let mutable param : ActorParam<'msg> = noActorParam
    member __.Spawner = spawner
    member __.Args = args
    member __.WrapReq = wrapReq
    member __.CastEvt = castEvt
    member __.Logic = inUse <- true ; logic
    member __.Param = inUse <- true ; param
    member this.WithSubscribe subscribe =
        if inUse then
            failWith "Already_In_Use" subscribe
        logic <-
            {
                Init = logic.Init
                Update = logic.Update
                Subscribe = subscribe
            }
        this
    member this.WithParam (update : ActorParam<'msg> -> ActorParam<'msg>) =
        if inUse then
            failWith "Already_In_Use" (param, update)
        param <- update param
        this
    interface IActorSpec<'args, 'msg, 'req, 'evt> with
        member __.Args = args
        member __.WrapReq = wrapReq
        member __.CastEvt = castEvt

let withSubscribe (subscribe : Subscribe<'runner, 'model, 'msg>) (spec : ActorSpec<'runner, 'args, 'model, 'msg, 'req, 'evt>) =
    spec.WithSubscribe subscribe

let withWrapEvt (wrapEvt : Wrapper<'msg, AgentEvt>) (spec : ActorSpec<'runner, 'args, 'model, 'msg, 'req, 'evt>) =
    spec.WithParam (fun p -> p.WithWrapEvt wrapEvt)

[<StructuredFormatDisplay("<Actor>{AsDisplay}")>]
type internal Actor<'args, 'model, 'msg, 'req, 'evt
            when 'model : not struct and 'msg :> IMsg
                and 'req :> IReq and 'evt :> IEvt>
        (agent', spec', model') =
    let agent : IAgent<'msg> = agent'
    let spec : IActorSpec<'args, 'msg, 'req, 'evt> = spec'
    let onEvent = new Bus<'evt> (agent :> IOwner, "OnEvent")
    let mutable state : 'model = model'
    let mutable version : Version = Version.Init
    let tryFireEvent (msg : 'msg) =
        match spec.CastEvt msg with
        | None ->
            ()
        | Some evt ->
            version <- version.IncEvt
            onEvent.Trigger evt
    member __.AsDisplay = (agent.Ident, version)
    member __.State = state
    member __.SetState (msg : 'msg) (newState : 'model) =
        state <- newState
        version <- version.IncMsg (not (newState =? state))
        tryFireEvent msg
    //Fable has issue if use Handle as the name, will cause dead loop
    member __.Handle' (req : 'req) =
        version <- version.IncReq
        agent.Deliver (spec.WrapReq req)
#if !FABLE_COMPILER
    member __.HandleAsync (getReq : Callback<'res> -> 'req) =
        version <- version.IncReq
        agent.DeliverAsync (spec.WrapReq << getReq)
#endif
    member __.Version = version
    member this.AsActor3 = this :> IActor<'args, 'model, 'req, 'evt>
    member this.AsActor = this.AsActor3
    interface IActor with
        member __.Ident = agent.Ident
        member __.Version = version
    interface IActor<'req, 'evt> with
        member this.Handle req = this.Handle' req
#if !FABLE_COMPILER
        member this.HandleAsync getReq = this.HandleAsync getReq
#endif
        member __.OnEvent = onEvent.Publish
        member this.AsActor1 = this :> IActor
    interface IActor<'args, 'model, 'req, 'evt> with
        member __.Args = spec.Args
        member __.State = state
        member this.AsActor2 = this :> IActor<'req, 'evt>

let internal createActor'
        (spec : ActorSpec<'runner, 'args, 'model, 'msg, 'req, 'evt>)
        (wrapMsg : WrapMsg<'wrapRunner, 'agentModel, 'agentMsg>)
        (subscribeNow : bool)
        (runner : 'runner :> IAgent<'args, 'model, 'msg, 'req, 'evt>) =
    let agent = runner :> IAgent<'msg>
    let (model, cmd) = spec.Logic.Init agent spec.Args
    let actor = new Actor<'args, 'model, 'msg, 'req, 'evt> (agent, spec, model)
    let updateActor : Update<'wrapRunner, NoModel, 'msg> =
        fun _runner msg model' ->
            let (model, cmd) = spec.Logic.Update runner msg actor.State
            actor.SetState msg model
            (model', cmd)
    let wrapperSpec : WrapperSpec<'wrapRunner, 'agentModel, 'agentMsg, NoModel, 'msg> =
        {
            GetSub = fun m -> NoModel
            SetSub = fun s -> id
            UpdateSub = updateActor
            ReactSub = noReaction
        }
    let actor = actor.AsActor
    let wrapper = wrap wrapMsg wrapperSpec
    if subscribeNow then
        let cmd = batchCmd [cmd ; spec.Logic.Subscribe runner model]
        (actor, wrapper, cmd)
    else
        (actor, wrapper, cmd)
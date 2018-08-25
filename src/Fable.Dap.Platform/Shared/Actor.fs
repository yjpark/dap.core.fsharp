[<AutoOpen>]
module Dap.Platform.Actor

#if !FABLE_COMPILER
open System.Threading.Tasks
#endif

open Dap.Prelude
open Dap.Context

//Note: during Init, the model is not created yet
type ActorInit<'args, 'model, 'msg>
            when 'model : not struct and 'msg :> IMsg =
    Init<IAgent<'msg>, 'args, 'model, 'msg>

/// Change model according to msg, also may generate cmds.
type ActorUpdate<'runner, 'args, 'model, 'msg, 'req, 'evt>
            when 'runner :> IAgent<'args, 'model, 'msg, 'req, 'evt>
                    and 'model : not struct and 'msg :> IMsg
                    and 'req :> IReq and 'evt :> IEvt =
    Update<'runner, 'model, 'msg>

/// Generate msg from outside. e.g. an timer, or keyboard.
type ActorSubscribe<'runner, 'args, 'model, 'msg, 'req, 'evt>
            when 'runner :> IAgent<'args, 'model, 'msg, 'req, 'evt>
                    and 'model : not struct and 'msg :> IMsg
                    and 'req :> IReq and 'evt :> IEvt =
    Subscribe<'runner, 'model, 'msg>

type ActorOperate<'runner, 'args, 'model, 'msg, 'req, 'evt>
            when 'runner :> IAgent<'args, 'model, 'msg, 'req, 'evt>
                    and 'msg :> IMsg and 'req :> IReq and 'evt :> IEvt =
    Operate<'runner, 'model, 'msg>

type ActorParam<'msg> when 'msg :> IMsg = {
    WrapEvt : Wrapper<'msg, AgentEvt> option
#if !FABLE_COMPILER
    GetSlowCap : GetSlowCap option
#endif
} with
    member this.WithWrapEvt v = {this with WrapEvt = Some v}

#if !FABLE_COMPILER
    member this.WithGetSlowCap v = {this with GetSlowCap = Some v}
#endif

let noActorParam<'msg when 'msg :> IMsg> : ActorParam<'msg> =
    {
        WrapEvt = None
#if !FABLE_COMPILER
        GetSlowCap = None
#endif
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
    member _this.Spawner = spawner
    member _this.Args = args
    member _this.WrapReq = wrapReq
    member _this.CastEvt = castEvt
    member _this.Logic = inUse <- true ; logic
    member _this.Param = inUse <- true ; param
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
    member this.WithParam (param' : ActorParam<'msg>) =
        if inUse then
            failWith "Already_In_Use" (param, param')
        param <- param'
        this
    member this.WithParam (update : ActorParam<'msg> -> ActorParam<'msg>) =
        if inUse then
            failWith "Already_In_Use" (param, update)
        param <- update param
        this
    interface IActorSpec<'args, 'msg, 'req, 'evt> with
        member _this.Args = args
        member _this.WrapReq = wrapReq
        member _this.CastEvt = castEvt

[<StructuredFormatDisplay("<Actor>{AsDisplay}")>]
type internal Actor<'args, 'model, 'msg, 'req, 'evt
            when 'model : not struct and 'msg :> IMsg
                and 'req :> IReq and 'evt :> IEvt>
        (agent', spec', model') =
    let agent : IAgent<'msg> = agent'
    let spec : IActorSpec<'args, 'msg, 'req, 'evt> = spec'
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
    member this.AsActor = this :> IActor<'args, 'model, 'req, 'evt>
    interface IActor<'args, 'model, 'req, 'evt> with
        member this.Handle req = this.Handle req
#if !FABLE_COMPILER
        member this.HandleAsync getReq = this.HandleAsync getReq
#endif
        member _this.OnEvent = bus.Publish
        member _this.Ident = agent.Ident
        member _this.Args = spec.Args
        member _this.State = state
        member _this.Version = version

let internal create'
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
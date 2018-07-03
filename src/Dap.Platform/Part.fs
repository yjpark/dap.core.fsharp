[<AutoOpen>]
[<RequireQualifiedAccess>]
module Dap.Platform.Part

open Dap.Prelude

type IActorPart<'args, 'model, 'req, 'evt> when 'req :> IReq and 'evt :> IEvt =
    inherit IHandler<'req>
    inherit IChannel<'evt>
    abstract Args : 'args with get
    abstract State : 'model with get

type IPart<'args, 'model, 'msg, 'req, 'evt> when 'msg :> IMsg and 'req :> IReq and 'evt :> IEvt =
    inherit IAgent
    inherit IPoster<'req>
    inherit IAsyncPoster<'req>
    abstract Actor : IActorPart<'args, 'model, 'req, 'evt> with get
    abstract Deliver : 'msg -> unit
    abstract RunFunc4<'res> : Func<IPart<'args, 'model, 'msg, 'req, 'evt>, 'res> -> Result<'res, exn>
    abstract AddTask4 : OnFailed<IPart<'args, 'model, 'msg, 'req, 'evt>> -> GetTask<IPart<'args, 'model, 'msg, 'req, 'evt>, unit> -> unit

type PartInit<'args, 'model, 'msg> =
    Init<IAgent, 'args, 'model, 'msg>

type PartUpdate<'args, 'model, 'msg, 'req, 'evt> when 'msg :> IMsg and 'req :> IReq and 'evt :> IEvt =
    Update<IPart<'args, 'model, 'msg, 'req, 'evt>, 'model, 'msg>

type PartNewArgs<'args> = NewArgs<IAgent, 'args>

type PartSpec<'args, 'model, 'msg, 'req, 'evt> when 'msg :> IMsg and 'req :> IReq and 'evt :> IEvt = {
    Init : PartInit<'args, 'model, 'msg>
    Update : PartUpdate<'args, 'model, 'msg, 'req, 'evt>
    NewArgs : PartNewArgs<'args>
    WrapReq : Wrapper<'msg, 'req>
    CastEvt : CastEvt<'msg, 'evt>
}

type PartOperate<'args, 'model, 'msg, 'req, 'evt> when 'msg :> IMsg and 'req :> IReq and 'evt :> IEvt =
    Operate<IPart<'args, 'model, 'msg, 'req, 'evt>, 'model, 'msg>

type PartWrapping<'args, 'model, 'msg, 'req, 'evt> when 'msg :> IMsg and 'req :> IReq and 'evt :> IEvt =
    IWrapping<IPart<'args, 'model, 'msg, 'req, 'evt>, 'model, 'msg>

type IPart<'agentMsg, 'args, 'model, 'msg, 'req, 'evt> when 'agentMsg :> IMsg and 'msg :> IMsg and 'req :> IReq and 'evt :> IEvt =
    inherit IPart<'args, 'model, 'msg, 'req, 'evt>
    abstract Wrapper : Wrapper<'agentMsg, 'msg> with get

type internal Part<'agentMsg, 'args, 'model, 'msg, 'req, 'evt> when 'agentMsg :> IMsg and 'msg :> IMsg and 'req :> IReq and 'evt :> IEvt = {
    Spec : PartSpec<'args, 'model, 'msg, 'req, 'evt>
    Agent : IAgent
    mutable Wrapper : Wrapper<'agentMsg, 'msg> option
    mutable Dispatch : DispatchMsg<'msg> option
    mutable State : 'model option
    mutable Actor' : ActorPart<'agentMsg, 'args, 'model, 'msg, 'req, 'evt> option
} with
    static member Create spec agent =
        {
            Spec = spec
            Agent = agent
            Wrapper = None
            Dispatch = None
            State = None
            Actor' = None
        }
    member this.Actor =
        if this.Actor'.IsNone then
            this.Actor' <- Some <| ActorPart<'agentMsg, 'args, 'model, 'msg, 'req, 'evt>.Create this
        this.Actor'
        |> Option.get
    member this.Post (req : 'req) = dispatch' this (this.Spec.WrapReq req)
    member this.PostAsync (getReq : Callback<'res> -> 'req) = dispatchAsync' this (this.Spec.WrapReq << getReq)
    interface ILogger with
        member this.Log m = this.Agent.Log m
    interface IRunner with
        member this.Clock = this.Agent.Env.Clock
        member this.Stats = this.Agent.Stats
        member this.RunFunc func = this.Agent.RunFunc func
        member this.AddTask onFailed getTask = this.Agent.AddTask onFailed getTask
        member this.ScheduleTask task = this.Agent.ScheduleTask task
        member this.RunTasks () = this.Agent.RunTasks ()
        member this.ClearPendingTasks () = this.Agent.ClearPendingTasks ()
        member this.CancelRunningTasks () = this.Agent.CancelRunningTasks ()
        member this.PendingTasksCount = this.Agent.PendingTasksCount
        member this.RunningTasksCount = this.Agent.RunningTasksCount
    interface IOwner with
        member this.Ident = (this.Agent :> IOwner).Ident
        member this.Disposed = (this.Agent :> IOwner).Disposed
    interface IAgent with
        member this.Env = this.Agent.Env
        member this.Ident = this.Agent.Ident
        member this.Handle req = this.Agent.Handle req
        member this.HandleAsync getReq = this.Agent.HandleAsync getReq
        member this.RunFunc1<'res> func = this.Agent.RunFunc1<'res> func
        member this.AddTask1 onFailed getTask = this.Agent.AddTask1 onFailed getTask

    interface IDispatcher<'msg> with
        member this.Dispatch = this.Dispatch
        member this.SetDispatch dispatch = this.Dispatch <- Some dispatch

    interface IPart<'agentMsg, 'args, 'model, 'msg, 'req, 'evt> with
        member this.Wrapper = this.Wrapper |> Option.get
        member this.Post req = this.Post req
        member this.PostAsync req = this.PostAsync req
        member this.Actor = this.Actor :> IActorPart<'args, 'model, 'req, 'evt>
        member this.Deliver msg = dispatch' this msg
        member this.RunFunc4 func = runFunc' this func
        member this.AddTask4 onFailed getTask = addTask' this onFailed getTask

and internal ActorPart<'agentMsg, 'args, 'model, 'msg, 'req, 'evt> when 'agentMsg :> IMsg and 'msg :> IMsg and 'req :> IReq and 'evt :> IEvt = {
    Part : Part<'agentMsg, 'args, 'model, 'msg, 'req, 'evt>
    Args : 'args
    OnEvent : IBus<'evt>
    FireEvent' : 'evt -> unit
} with
    static member Create m =
        let args = m.Spec.NewArgs (m.Agent)
        let event = new Bus<'evt> (m.Agent :> IOwner)
        {
            Part = m
            Args = args
            OnEvent = event.Publish
            FireEvent' = event.Trigger
        }
    interface IActorPart<'args, 'model, 'req, 'evt> with
        member this.Handle req = this.Part.Post req
        member this.OnEvent = this.OnEvent
        member this.Args = this.Args
        member this.State =
            this.Part.State
            |> Option.get

let init (modMsg : Wrapper<'agentMsg, 'msg>)
        (wrapMsg : WrapMsg<IAgent<'agentMsg, 'agentReq, 'agentEvt>, 'AgentModel, 'agentMsg>)
        (getPart : 'AgentModel -> 'model)
        (setPart : 'model -> 'AgentModel -> 'AgentModel)
        (agent : IAgent<'agentMsg, 'agentReq, 'agentEvt>)
        (modSpec : PartSpec<'args, 'model, 'msg, 'req, 'evt>)
        : IPart<'agentMsg, 'args, 'model, 'msg, 'req, 'evt> =
    let part = Part<'agentMsg, 'args, 'model, 'msg, 'req, 'evt>.Create modSpec agent
    let part' = part :> IPart<'args, 'model, 'msg, 'req, 'evt>

    let updatePart : Update<'agent, 'model, 'msg> =
        fun _runner model msg ->
            let (model, cmd) = modSpec.Update part' model msg
            part.State <- Some model
            (model, cmd)

    let reactPart : React<IAgent<'agentMsg, 'agentReq, 'agentEvt>, 'AgentModel, 'agentMsg, 'model, 'msg> =
        fun _runner msg _model agentModel ->
            match modSpec.CastEvt msg with
            | Some evt ->
                part.Actor.FireEvent' evt
            | None ->
                ()
            (agentModel, noCmd)

    let wrapperSpec : WrapperSpec<'agent, 'AgentModel, 'agentMsg, 'model, 'msg> =
        {
            GetSub = getPart
            SetSub = setPart
            UpdateSub = updatePart
            ReactSub = reactPart
        }
    part.Wrapper <- Some <| wrap wrapMsg wrapperSpec
    (part :> IDispatcher<'msg>).SetDispatch (fun (time, msg) ->
        agent.Deliver <| modMsg msg
    )
    let (model, cmd) = modSpec.Init (part' :> IAgent) part'.Actor.Args
    part.State <- Some model
    cmd |> List.iter (fun m -> m <| dispatch' part)
    part :> IPart<'agentMsg, 'args, 'model, 'msg, 'req, 'evt>

let replyAsync4 (runner : IPart<'args, 'model, 'msg, 'req, 'evt>) (req : IReq) (callback : Callback<'res>)
                (getOnFailed: OnReplyFailed<IPart<'args, 'model, 'msg, 'req, 'evt>, 'res>)
                (getReplyTask : GetReplyTask<IPart<'args, 'model, 'msg, 'req, 'evt>, 'res>) : unit =
    let onFailed = getOnFailed req callback
    let getTask = getReplyTask req callback
    runner.AddTask4 onFailed getTask

[<AutoOpen>]
[<RequireQualifiedAccess>]
module Dap.Platform.Part

open Dap.Prelude

type IPart<'args, 'model, 'msg, 'req, 'evt> when 'msg :> IMsg and 'req :> IReq and 'evt :> IEvt =
    inherit IAgent<'msg>
    inherit IPoster<'req>
    inherit IAsyncPoster<'req>
    abstract Agent : IAgent with get
    abstract Actor : IActor<'args, 'model, 'req, 'evt> with get
    abstract RunFunc4<'res> : Func<IPart<'args, 'model, 'msg, 'req, 'evt>, 'res> -> Result<'res, exn>
    abstract AddTask4 : OnFailed<IPart<'args, 'model, 'msg, 'req, 'evt>> -> GetTask<IPart<'args, 'model, 'msg, 'req, 'evt>, unit> -> unit
    abstract RunTask4 : OnFailed<IPart<'args, 'model, 'msg, 'req, 'evt>> -> GetTask<IPart<'args, 'model, 'msg, 'req, 'evt>, unit> -> unit

type PartInit<'args, 'model, 'msg> =
    Init<IAgent, 'args, 'model, 'msg>

type PartUpdate<'args, 'model, 'msg, 'req, 'evt> when 'msg :> IMsg and 'req :> IReq and 'evt :> IEvt =
    Update<IPart<'args, 'model, 'msg, 'req, 'evt>, 'model, 'msg>

type PartSpec<'args, 'model, 'msg, 'req, 'evt> when 'msg :> IMsg and 'req :> IReq and 'evt :> IEvt (newArgs', wrapReq', castEvt', init', update') =
    inherit ActorSpec'<IAgent, IPart<'args, 'model, 'msg, 'req, 'evt>, 'args, 'model, 'msg, 'req, 'evt>(newArgs', wrapReq', castEvt', init', update', noSubscription)

type PartOperate<'args, 'model, 'msg, 'req, 'evt> when 'msg :> IMsg and 'req :> IReq and 'evt :> IEvt =
    Operate<IPart<'args, 'model, 'msg, 'req, 'evt>, 'model, 'msg>

type PartWrapMsg<'actorModel, 'actorMsg> when 'actorMsg :> IMsg =
    WrapMsg<IAgent, 'actorModel, 'actorMsg>

type PartWrapping<'actorModel, 'actorMsg> when 'actorMsg :> IMsg =
    IWrapping<IAgent, 'actorModel, 'actorMsg>

type IPart<'actorMsg, 'args, 'model, 'msg, 'req, 'evt> when 'actorMsg :> IMsg and 'msg :> IMsg and 'req :> IReq and 'evt :> IEvt =
    inherit IPart<'args, 'model, 'msg, 'req, 'evt>
    abstract Wrapper : Wrapper<'actorMsg, 'msg> with get

[<StructuredFormatDisplay("<Part>{AsDisplay}")>]
type internal Part<'actorMsg, 'args, 'model, 'msg, 'req, 'evt when 'actorMsg :> IMsg and 'model : not struct and 'msg :> IMsg and 'req :> IReq and 'evt :> IEvt> (spec', partMsg', agent') =
    let spec : PartSpec<'args, 'model, 'msg, 'req, 'evt> = spec'
    let partMsg : Wrapper<'actorMsg, 'msg> = partMsg'
    let agent : IAgent<'actorMsg> = agent'
    let mutable actor : Actor<'args, 'model, 'msg, 'req, 'evt> option = None
    let mutable wrapper : Wrapper<'actorMsg, 'msg> option = None
    let mutable dispatch : DispatchMsg<'msg> option = None
    member _this.AsDisplay = (agent.Ident, partMsg)
    member this.AsPart = this :> IPart<'args, 'model, 'msg, 'req, 'evt>
    member this.Setup (wrapMsg : PartWrapMsg<'actorModel, 'actorMsg>) =
        if actor.IsSome then
            raiseWithError "Agent" "Already_Setup" (actor)
        let args : 'args = spec.NewArgs (agent :> IOwner)
        let (model, cmd) = spec.Logic.Init (this :> IAgent) args
        let actor' = new Actor<'args, 'model, 'msg, 'req, 'evt> (this, spec, args, model)
        actor <- Some actor'
        let runner = this.AsPart
        let updateActor : Update<IAgent, NoModel, 'msg> =
            fun _runner model' msg ->
                let (model, cmd) = spec.Logic.Update runner actor'.State msg
                actor'.SetState msg model
                (model', cmd)
        let wrapperSpec : WrapperSpec<IAgent, 'actorModel, 'actorMsg, NoModel, 'msg> =
            {
                GetSub = fun m -> NoModel
                SetSub = fun s -> id
                UpdateSub = updateActor
                ReactSub = noReaction
            }
        wrapper <- Some <| wrap wrapMsg wrapperSpec
        (this :> IDispatcher<'msg>).SetDispatch (fun (_time, msg) ->
            agent.Deliver <| partMsg msg
        )
        cmd |> List.iter (fun m -> m <| dispatch' this)
    member _this.Actor = actor |> Option.get
    member this.Post (req : 'req) = this.Actor.Handle req
    member this.PostAsync (getReq : Callback<'res> -> 'req) = this.Actor.HandleAsync getReq
    interface ILogger with
        member _this.Log m = agent.Log m
    interface IRunner with
        member _this.Clock = agent.Env.Clock
        member _this.Stats = agent.Stats
        member _this.RunFunc func = agent.RunFunc func
        member _this.AddTask onFailed getTask = agent.AddTask onFailed getTask
        member _this.RunTask onFailed getTask = agent.RunTask onFailed getTask
    interface ITaskManager with
        member this.StartTask task = agent.StartTask task
        member this.ScheduleTask task = agent.ScheduleTask task
        member this.PendingTasksCount = agent.PendingTasksCount
        member this.StartPendingTasks () = agent.StartPendingTasks ()
        member this.ClearPendingTasks () = agent.ClearPendingTasks ()
        member this.RunningTasksCount = agent.RunningTasksCount
        member this.CancelRunningTasks () = agent.CancelRunningTasks ()

    interface IOwner with
        member _this.Ident = (agent :> IOwner).Ident
        member _this.Disposed = (agent :> IOwner).Disposed
    interface IAgent with
        member _this.Env = agent.Env
        member _this.Ident = agent.Ident
        member _this.Handle req = agent.Handle req
        member _this.HandleAsync getReq = agent.HandleAsync getReq
        member _this.RunFunc1<'res> func = agent.RunFunc1<'res> func
        member _this.AddTask1 onFailed getTask = agent.AddTask1 onFailed getTask
        member _this.RunTask1 onFailed getTask = agent.RunTask1 onFailed getTask
    interface IDispatcher<'msg> with
        member _this.Dispatch = dispatch
        member _this.SetDispatch dispatch' = dispatch <- Some dispatch'
    interface IPart<'actorMsg, 'args, 'model, 'msg, 'req, 'evt> with
        member _this.Wrapper = wrapper |> Option.get
        member this.Post req = this.Post req
        member this.PostAsync getReq = this.PostAsync getReq
        member this.Agent = agent :> IAgent
        member this.Actor = this.Actor :> IActor<'args, 'model, 'req, 'evt>
        member this.Deliver msg = dispatch' this msg
        member this.DeliverAsync getMsg = dispatchAsync' this getMsg
        member this.RunFunc4 func = runFunc' this func
        member this.AddTask4 onFailed getTask = addTask' this onFailed getTask
        member this.RunTask4 onFailed getTask = runTask' this onFailed getTask

let init (spec : PartSpec<'args, 'model, 'msg, 'req, 'evt>)
        (partMsg : Wrapper<'actorMsg, 'msg>)
        (wrapMsg : PartWrapMsg<'actorModel, 'actorMsg>)
        (agent : IAgent<'actorMsg>)
        : IPart<'actorMsg, 'args, 'model, 'msg, 'req, 'evt> =
    let part = new Part<'actorMsg, 'args, 'model, 'msg, 'req, 'evt> (spec, partMsg, agent)
    part.Setup wrapMsg
    part :> IPart<'actorMsg, 'args, 'model, 'msg, 'req, 'evt>

let replyAsync4 (runner : IPart<'args, 'model, 'msg, 'req, 'evt>) (req : IReq) (callback : Callback<'res>)
                (getOnFailed: OnReplyFailed<IPart<'args, 'model, 'msg, 'req, 'evt>, 'res>)
                (getReplyTask : GetReplyTask<IPart<'args, 'model, 'msg, 'req, 'evt>, 'res>) : unit =
    let onFailed = getOnFailed req callback
    let getTask = getReplyTask req callback
    runner.AddTask4 onFailed getTask

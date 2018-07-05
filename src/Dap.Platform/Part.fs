[<AutoOpen>]
[<RequireQualifiedAccess>]
module Dap.Platform.Part

open Dap.Prelude

type IPart<'actorMsg, 'args, 'model, 'msg, 'req, 'evt> when 'actorMsg :> IMsg and 'msg :> IMsg and 'req :> IReq and 'evt :> IEvt =
    inherit IAgent<'args, 'model, 'msg, 'req, 'evt>
    abstract Agent : IAgent<'actorMsg> with get
    abstract Wrapper : Wrapper<'actorMsg, 'msg> with get
    abstract RunFunc4<'res> : Func<IPart<'actorMsg, 'args, 'model, 'msg, 'req, 'evt>, 'res> -> Result<'res, exn>
    abstract AddTask4 : OnFailed<IPart<'actorMsg, 'args, 'model, 'msg, 'req, 'evt>> -> GetTask<IPart<'actorMsg, 'args, 'model, 'msg, 'req, 'evt>, unit> -> unit
    abstract RunTask4 : OnFailed<IPart<'actorMsg, 'args, 'model, 'msg, 'req, 'evt>> -> GetTask<IPart<'actorMsg, 'args, 'model, 'msg, 'req, 'evt>, unit> -> unit

[<StructuredFormatDisplay("<Part>{AsDisplay}")>]
type internal Part<'actorMsg, 'args, 'model, 'msg, 'req, 'evt when 'actorMsg :> IMsg and 'model : not struct and 'msg :> IMsg and 'req :> IReq and 'evt :> IEvt> (spec', partMsg', agent') =
    let spec : ActorSpec<'args, 'model, 'msg, 'req, 'evt> = spec'
    let partMsg : Wrapper<'actorMsg, 'msg> = partMsg'
    let agent : IAgent<'actorMsg> = agent'
    let mutable actor : Actor<'args, 'model, 'msg, 'req, 'evt> option = None
    let mutable wrapper : Wrapper<'actorMsg, 'msg> option = None
    let mutable dispatch : DispatchMsg<'msg> option = None
    member _this.AsDisplay = (agent.Ident, partMsg)
    member this.AsPart = this :> IPart<'actorMsg, 'args, 'model, 'msg, 'req, 'evt>
    member this.AsAgent = this :> IAgent<'args, 'model, 'msg, 'req, 'evt>
    member this.Setup (wrapMsg : ActorWrapMsg<'actorModel, 'actorMsg>) =
        if actor.IsSome then
            raiseWithError "Part" "Already_Setup" (actor)
        let (actor', cmd, wrapper') = this.AsAgent |> create' spec wrapMsg true
        actor <- Some actor'
        wrapper <- Some <| wrapper'
        (this :> IDispatcher<'msg>).SetDispatch (fun (_time, msg) ->
            agent.Deliver <| partMsg msg
        )
        cmd |> List.iter (fun m -> m <| dispatch' this)
    member _this.Actor = actor |> Option.get
    member this.Post (req : 'req) = this.Actor.Handle req
    member this.PostAsync (getReq : Callback<'res> -> 'req) = this.Actor.HandleAsync getReq
    interface IDispatcher<'msg> with
        member _this.Dispatch = dispatch
        member _this.SetDispatch dispatch' = dispatch <- Some dispatch'
    interface ILogger with
        member _this.Log m = agent.Log m
    interface IRunner with
        member _this.Clock = agent.Env.Clock
        member _this.Stats = agent.Stats
        member this.RunFunc func = runFunc' this func
        member this.AddTask onFailed getTask = addTask' this onFailed getTask
        member this.RunTask onFailed getTask = runTask' this onFailed getTask
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
        member this.RunFunc1 func = runFunc' this func
        member this.AddTask1 onFailed getTask = addTask' this onFailed getTask
        member this.RunTask1 onFailed getTask = runTask' this onFailed getTask
    interface IAgent<'req, 'evt> with
        member this.Post req = this.Post req
        member this.PostAsync getReq = this.PostAsync getReq
        member this.Actor = this.Actor :> IActor<'req, 'evt>
        member this.RunFunc2 func = runFunc' this func
        member this.AddTask2 onFailed getTask = addTask' this onFailed getTask
        member this.RunTask2 onFailed getTask = runTask' this onFailed getTask
    interface IAgent<'msg> with
        member this.Deliver msg = dispatch' this msg
        member this.DeliverAsync getMsg = dispatchAsync' this getMsg
    interface IAgent<'args, 'model, 'msg, 'req, 'evt> with
        member this.Actor = this.Actor :> IActor<'args, 'model, 'req, 'evt>
        member this.RunFunc3 func = runFunc' this func
        member this.AddTask3 onFailed getTask = addTask' this onFailed getTask
        member this.RunTask3 onFailed getTask = runTask' this onFailed getTask
    interface IPart<'actorMsg, 'args, 'model, 'msg, 'req, 'evt> with
        member _this.Agent = agent
        member _this.Wrapper = wrapper |> Option.get
        member this.RunFunc4 func = runFunc' this func
        member this.AddTask4 onFailed getTask = addTask' this onFailed getTask
        member this.RunTask4 onFailed getTask = runTask' this onFailed getTask

let create (spec : ActorSpec<'args, 'model, 'msg, 'req, 'evt>)
        (partMsg : Wrapper<'actorMsg, 'msg>)
        (wrapMsg : ActorWrapMsg<'actorModel, 'actorMsg>)
        (agent : IAgent<'actorMsg>)
        : IPart<'actorMsg, 'args, 'model, 'msg, 'req, 'evt> =
    let part = new Part<'actorMsg, 'args, 'model, 'msg, 'req, 'evt> (spec, partMsg, agent)
    part.Setup wrapMsg
    part :> IPart<'actorMsg, 'args, 'model, 'msg, 'req, 'evt>

let replyAsync4 (runner : IPart<'actorMsg, 'args, 'model, 'msg, 'req, 'evt>) (req : IReq) (callback : Callback<'res>)
                (getOnFailed: OnReplyFailed<IPart<'actorMsg, 'args, 'model, 'msg, 'req, 'evt>, 'res>)
                (getReplyTask : GetReplyTask<IPart<'actorMsg, 'args, 'model, 'msg, 'req, 'evt>, 'res>) : unit =
    let onFailed = getOnFailed req callback
    let getTask = getReplyTask req callback
    runner.AddTask4 onFailed getTask

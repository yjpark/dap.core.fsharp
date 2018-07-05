module Dap.Platform.Internal.Agent

open Dap.Prelude
open Dap.Platform

type internal AgentModel<'args, 'model, 'msg, 'req, 'evt> when 'model : not struct and 'msg :> IMsg and 'req :> IReq and 'evt :> IEvt = {
    Spec : ActorSpec<'args, 'model, 'msg, 'req, 'evt>
    Actor : Actor<'args, 'model, 'msg, 'req, 'evt>
    Wrapper : Wrapper<AgentMsg<'args, 'model, 'msg, 'req, 'evt>, 'msg>
}

and internal AgentMsg<'args, 'model, 'msg, 'req, 'evt> when 'model : not struct and 'msg :> IMsg and 'req :> IReq and 'evt :> IEvt =
    | AgentReq of AgentReq
    | AgentEvt of AgentEvt
    | ActorMsg of 'msg
    | ActorMsg' of AgentWrapping<'args, 'model, 'msg, 'req, 'evt>
with interface IMsg

and internal AgentWrapping<'args, 'model, 'msg, 'req, 'evt> when 'model : not struct and 'msg :> IMsg and 'req :> IReq and 'evt :> IEvt =
    ActorWrapping<AgentModel<'args, 'model, 'msg, 'req, 'evt>, AgentMsg<'args, 'model, 'msg, 'req, 'evt>>

type internal AgentLogic<'args, 'model, 'msg, 'req, 'evt> when 'model : not struct and 'msg :> IMsg and 'req :> IReq and 'evt :> IEvt =
    Logic<IAgent<'args, 'model, 'msg, 'req, 'evt>,
            IAgent<'args, 'model, 'msg, 'req, 'evt>,
            NoArgs,
            AgentModel<'args, 'model, 'msg, 'req, 'evt>,
            AgentMsg<'args, 'model, 'msg, 'req, 'evt>>

[<StructuredFormatDisplay("<Agent>{AsDisplay}")>]
type internal Agent<'args, 'model, 'msg, 'req, 'evt when 'model : not struct and 'msg :> IMsg and 'req :> IReq and 'evt :> IEvt> (spec', env', ident', logger', logic') =
    let spec : ActorSpec<'args, 'model, 'msg, 'req, 'evt> = spec'
    let env : IEnv = env'
    let ident : Ident = ident'
    let mutable logger : ILogger = logger'
    let stats : Stats =
        spec.Param.GetSlowCap
        |> Option.defaultValue getDefaultSlowCap
        |> statsOfCap
    let taskManager : ITaskManager = new TaskManager () :> ITaskManager
    let mutable logic : AgentLogic<'args, 'model, 'msg, 'req, 'evt> = logic'
    let mutable disposed : bool = false
    let mutable dispatch : DispatchMsg<AgentMsg<'args, 'model, 'msg, 'req, 'evt>> option = None
    let mutable state : AgentModel<'args, 'model, 'msg, 'req, 'evt> option = None
    let mutable version : Version = Version.Init
    member this.AsDisplay = (ident, version, this.Actor)
    member this.AsAgent = this :> IAgent<'args, 'model, 'msg, 'req, 'evt>
    member _this.Actor = state |> Option.get |> fun m -> m.Actor
    member _this.SetState (newState : AgentModel<'args, 'model, 'msg, 'req, 'evt>) =
        let stateChanged = state.IsNone || not (newState =? Option.get state)
        state <- Some newState
        version <- version.IncMsg stateChanged
    member this.Handle (req : AgentReq) =
        version <- version.IncReq
        dispatch' this (AgentReq req)
    member this.HandleAsync (getReq : Callback<'res> -> AgentReq) =
        version <- version.IncReq
        dispatchAsync' this (AgentReq << getReq)
    member this.Post (subReq : 'req) = this.Actor.Handle subReq
    member this.PostAsync (getSubReq : Callback<'res> -> 'req) = this.Actor.HandleAsync getSubReq
    interface IRunnable<IAgent<'args, 'model, 'msg, 'req, 'evt>,
                        IAgent<'args, 'model, 'msg, 'req, 'evt>,
                        NoArgs,
                        AgentModel<'args, 'model, 'msg, 'req, 'evt>,
                        AgentMsg<'args, 'model, 'msg, 'req, 'evt>> with
        member _this.Args = NoArgs
        member _this.Logic = logic
        member _this.Dispatch = dispatch
        member _this.State = state
        member _this.SetDispatch dispatch' = dispatch <- Some dispatch'
        member this.Start () =
            if state.IsNone then
                logger <- enrichLoggerForAgent this logger
            start' this this.SetState
        member this.Process parcel = process' this parcel this.SetState
        member this.Deliver cmd = deliver' this cmd
        member this.Initer = this.AsAgent
        member this.Runner = this.AsAgent
    interface IRunner with
        member _this.Clock = env.Clock
        member _this.Stats = stats
        member this.RunFunc func = runFunc' this func
        member this.AddTask onFailed getTask = addTask' this onFailed getTask
        member this.RunTask onFailed getTask = runTask' this onFailed getTask
    interface ITaskManager with
        member this.StartTask task = taskManager.StartTask task
        member this.ScheduleTask task = taskManager.ScheduleTask task
        member this.PendingTasksCount = taskManager.PendingTasksCount
        member this.StartPendingTasks () = taskManager.StartPendingTasks ()
        member this.ClearPendingTasks () = taskManager.ClearPendingTasks ()
        member this.RunningTasksCount = taskManager.RunningTasksCount
        member this.CancelRunningTasks () = taskManager.CancelRunningTasks ()
    interface ILogger with
        member _this.Log m = logger.Log m
    interface IOwner with
        member _this.Ident = ident.Ident
        member _this.Disposed = disposed
    interface IAgent with
        member _this.Env = env
        member _this.Ident = ident
        member this.Handle req = this.Handle req
        member this.HandleAsync getReq = this.HandleAsync getReq
        member this.RunFunc1 func = runFunc' this func
        member this.AddTask1 onFailed getTask = addTask' this onFailed getTask
        member this.RunTask1 onFailed getTask = runTask' this onFailed getTask
    interface IAgent<'req, 'evt> with
        member this.Actor = this.Actor :> IActor<'req, 'evt>
        member this.Post subReq = this.Actor.Handle subReq
        member this.PostAsync getSubReq = this.PostAsync getSubReq
        member this.RunFunc2 func = runFunc' this func
        member this.AddTask2 onFailed getTask = addTask' this onFailed getTask
        member this.RunTask2 onFailed getTask = runTask' this onFailed getTask
    interface IAgent<'msg> with
        member this.Deliver (msg : 'msg) = dispatch' this (ActorMsg msg)
        member this.DeliverAsync (getMsg : Callback<'res> -> 'msg) = dispatchAsync' this (ActorMsg << getMsg)
    interface IAgent<'args, 'model, 'msg, 'req, 'evt> with
        member this.Actor = this.Actor :> IActor<'args, 'model, 'req, 'evt>

        member this.RunFunc3 func = runFunc' this func
        member this.AddTask3 onFailed getTask = addTask' this onFailed getTask
        member this.RunTask3 onFailed getTask = runTask' this onFailed getTask
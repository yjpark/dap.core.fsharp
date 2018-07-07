[<AutoOpen>]
module Dap.Platform.BaseAgent

open Elmish

open Dap.Prelude
open Dap.Platform.Internal.Env

type AgentModel<'runner, 'args, 'model, 'msg, 'req, 'evt>
            when 'runner :> IAgent<'args, 'model, 'msg, 'req, 'evt>
                    and 'model : not struct and 'msg :> IMsg
                    and 'req :> IReq and 'evt :> IEvt = {
    Spec : ActorSpec<'runner, 'args, 'model, 'msg, 'req, 'evt>
    Actor : IActor<'args, 'model, 'req, 'evt>
    Wrapper : Wrapper<AgentMsg<'runner, 'args, 'model, 'msg, 'req, 'evt>, 'msg>
}

and AgentMsg<'runner, 'args, 'model, 'msg, 'req, 'evt>
            when 'runner :> IAgent<'args, 'model, 'msg, 'req, 'evt>
                    and 'model : not struct and 'msg :> IMsg
                    and 'req :> IReq and 'evt :> IEvt =
    | AgentReq of AgentReq
    | AgentEvt of AgentEvt
    | ActorMsg of 'msg
    | ActorMsg' of AgentWrapping<'runner, 'args, 'model, 'msg, 'req, 'evt>
with interface IMsg

and AgentWrapping<'runner, 'args, 'model, 'msg, 'req, 'evt>
            when 'runner :> IAgent<'args, 'model, 'msg, 'req, 'evt>
                    and 'model : not struct and 'msg :> IMsg
                    and 'req :> IReq and 'evt :> IEvt =
    IWrapping<'runner, AgentModel<'runner, 'args, 'model, 'msg, 'req, 'evt>, AgentMsg<'runner, 'args, 'model, 'msg, 'req, 'evt>>

type AgentLogic<'runner, 'args, 'model, 'msg, 'req, 'evt>
            when 'runner :> IAgent<'args, 'model, 'msg, 'req, 'evt>
                    and 'model : not struct and 'msg :> IMsg
                    and 'req :> IReq and 'evt :> IEvt =
    Logic<'runner, 'runner, NoArgs,
            AgentModel<'runner, 'args, 'model, 'msg, 'req, 'evt>,
            AgentMsg<'runner, 'args, 'model, 'msg, 'req, 'evt>>

[<StructuredFormatDisplay("<Agent>{AsDisplay}")>]
[<AbstractClass>]
type BaseAgent<'runner, 'args, 'model, 'msg, 'req, 'evt
            when 'runner :> IAgent<'args, 'model, 'msg, 'req, 'evt>
                    and 'model : not struct and 'msg :> IMsg
                    and 'req :> IReq and 'evt :> IEvt>
        (param : AgentParam) =
    let env : IEnv = param.Env
    let ident : Ident = Ident.Create param.Env.Scope param.Kind param.Key
    let mutable logger : ILogger = env.Logging.GetLogger ident.Ident
    let mutable spec : ActorSpec<'runner, 'args, 'model, 'msg, 'req, 'evt> option = None
    let mutable logic : AgentLogic<'runner, 'args, 'model, 'msg, 'req, 'evt> option = None
    let mutable stats : Stats = statsOfCap getDefaultSlowCap
    let taskManager : ITaskManager = new TaskManager () :> ITaskManager
    let mutable disposed : bool = false
    let mutable dispatch : DispatchMsg<AgentMsg<'runner, 'args, 'model, 'msg, 'req, 'evt>> option = None
    let mutable state : AgentModel<'runner, 'args, 'model, 'msg, 'req, 'evt> option = None
    let mutable version : Version = Version.Init
    member this.AsDisplay = (ident, version, this.Actor)
    member private this.AsAgent = this :> IAgent<'args, 'model, 'msg, 'req, 'evt>
    member _this.Setup' spec' logic' =
        if spec.IsSome then
            raiseWithError "BaseAgent" "Already_Setup" (spec, spec')
        spec <- Some spec'
        logic <- Some logic'
        spec'.Param.GetSlowCap
        |> Option.iter (fun getSlowCap ->
            stats <- statsOfCap getSlowCap
        )
    member _this.SetState (newState : AgentModel<'runner, 'args, 'model, 'msg, 'req, 'evt>) =
        let stateChanged = state.IsNone || not (newState =? Option.get state)
        state <- Some newState
        version <- version.IncMsg stateChanged

    interface IRunnable<'runner, 'runner, NoArgs,
                        AgentModel<'runner, 'args, 'model, 'msg, 'req, 'evt>,
                        AgentMsg<'runner, 'args, 'model, 'msg, 'req, 'evt>> with
        member _this.Args = NoArgs
        member _this.Logic = logic |> Option.get
        member _this.Dispatch = dispatch
        member _this.State = state
        member _this.SetDispatch dispatch' = dispatch <- Some dispatch'
        member this.Start () =
            if state.IsNone then
                logger <- enrichLoggerForAgent this logger
            start' this this.SetState
        member this.Process parcel = process' this parcel this.SetState
        member this.Deliver cmd = deliver' this cmd
        member this.Initer = this.Runner
        member this.Runner = this.Runner
    //IRunner
    member this.Clock = env.Clock
    interface IRunner with
        member this.Clock = this.Clock
        member _this.Stats = stats
        member this.RunFunc0 func = runFunc' this func
        member this.AddTask0 onFailed getTask = addTask' this onFailed getTask
        member this.RunTask0 onFailed getTask = runTask' this onFailed getTask
    interface ITaskManager with
        member this.StartTask task = taskManager.StartTask task
        member this.ScheduleTask task = taskManager.ScheduleTask task
        member this.PendingTasksCount = taskManager.PendingTasksCount
        member this.StartPendingTasks () = taskManager.StartPendingTasks ()
        member this.ClearPendingTasks () = taskManager.ClearPendingTasks ()
        member this.RunningTasksCount = taskManager.RunningTasksCount
        member this.CancelRunningTasks () = taskManager.CancelRunningTasks ()
    //ILogger
    member _this.Log m = logger.Log m
    interface ILogger with
        member this.Log m = this.Log m
    interface IOwner with
        member _this.Ident = ident.Ident
        member _this.Disposed = disposed
    //IRunner<'runner>
    abstract member Runner : 'runner with get
    member this.RunFunc (func : Func<'runner, 'res>) = runFunc' this.Runner func
    member this.AddTask (onFailed : OnFailed<'runner>) (getTask : GetTask<'runner, unit>) = addTask' this.Runner onFailed getTask
    member this.RunTask (onFailed : OnFailed<'runner>) (getTask : GetTask<'runner, unit>) = runTask' this.Runner onFailed getTask
    interface IRunner<'runner> with
        member this.Runner = this.Runner
        member this.RunFunc func = this.RunFunc func
        member this.AddTask onFailed getTask = this.AddTask onFailed getTask
        member this.RunTask onFailed getTask = this.RunTask onFailed getTask
    //IAgent<'args, 'model, 'msg, 'req, 'evt>
    member _this.Actor = state |> Option.get |> fun m -> m.Actor
    interface IAgent<'args, 'model, 'msg, 'req, 'evt> with
        member this.Actor = this.Actor

        member this.RunFunc3 func = runFunc' this func
        member this.AddTask3 onFailed getTask = addTask' this onFailed getTask
        member this.RunTask3 onFailed getTask = runTask' this onFailed getTask
    //IAgent<'req, 'evt>
    member this.Post (subReq : 'req) = this.Actor.Handle subReq
    member this.PostAsync (getSubReq : Callback<'res> -> 'req) = this.Actor.HandleAsync getSubReq
    interface IAgent<'req, 'evt> with
        member this.Actor = this.Actor :> IActor<'req, 'evt>
        member this.Post subReq = this.Post subReq
        member this.PostAsync getSubReq = this.PostAsync getSubReq
        member this.RunFunc2 func = runFunc' this func
        member this.AddTask2 onFailed getTask = addTask' this onFailed getTask
        member this.RunTask2 onFailed getTask = runTask' this onFailed getTask
    //IAgent<'msg>
    member this.Deliver (msg : 'msg) = dispatch' this (ActorMsg msg)
    member this.DeliverAsync (getMsg : Callback<'res> -> 'msg) = dispatchAsync' this (ActorMsg << getMsg)
    interface IAgent<'msg> with
        member this.Deliver msg = this.Deliver msg
        member this.DeliverAsync getMsg = this.DeliverAsync getMsg
    //IAgent
    member _this.Env = env
    member _this.Ident = ident
    member this.Handle (req : AgentReq) =
        version <- version.IncReq
        dispatch' this (AgentReq req)
    member this.HandleAsync (getReq : Callback<'res> -> AgentReq) =
        version <- version.IncReq
        dispatchAsync' this (AgentReq << getReq)
    interface IAgent with
        member this.Env = this.Env
        member this.Ident = this.Ident
        member this.Handle req = this.Handle req
        member this.HandleAsync getReq = this.HandleAsync getReq
        member this.RunFunc1 func = runFunc' this func
        member this.AddTask1 onFailed getTask = addTask' this onFailed getTask
        member this.RunTask1 onFailed getTask = runTask' this onFailed getTask

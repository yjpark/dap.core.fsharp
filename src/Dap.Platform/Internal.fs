namespace Dap.Platform.Internal

open Dap.Prelude
open Dap.Platform

type internal EnvLogic =
    Logic<IEnv, IEnv, NoArgs, EnvModel, EnvMsg>

[<StructuredFormatDisplay("<Env>{AsDisplay}")>]
type internal Env (param' : EnvParam, logic') =
    let param : EnvParam = param'
    let logger : ILogger = param.Logging.GetLogger param.Scope
    let logic : EnvLogic = logic'
    let stats : Stats = statsOfCap <| defaultArg param.GetSlowCap getDefaultSlowCap
    let taskManager : ITaskManager = new TaskManager () :> ITaskManager
    let mutable dispatch : DispatchMsg<EnvMsg> option = None
    let mutable state : EnvModel option = None
    let mutable version : Version = Version.Init
    member _this.AsDisplay = (param.Scope, version)
    member this.SetState (newState : EnvModel) =
        let stateChanged = state.IsNone || not (newState =? Option.get state)
        state <- Some newState
        version <- version.IncMsg stateChanged
    member this.Handle (req : EnvReq) = dispatch' this (EnvReq req)
    member this.HandleAsync (getReq : Callback<'res> -> EnvReq) = dispatchAsync' this (EnvReq << getReq)
    interface IRunnable<IEnv, IEnv, NoArgs, EnvModel, EnvMsg> with
        member _this.Args = NoArgs
        member this.Logic = logic
        member this.Dispatch = dispatch
        member this.State = state
        member this.SetDispatch dispatch' = dispatch <- Some dispatch'
        member this.Start () = start' this this.SetState
        member this.Process parcel = process' this parcel this.SetState
        member this.Deliver msg = deliver' this msg
        member this.Initer = this :> IEnv
        member this.Runner = this :> IEnv
    interface IRunner with
        member this.Log m = logger.Log m
        member this.Clock = param.Clock
        member this.Stats = stats
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
    interface IEnv with
        member this.Platform = param.Platform
        member this.Logging = param.Logging
        member this.Scope = param.Scope
        member this.State = state
        member this.Handle req = this.Handle req
        member this.HandleAsync getReq = this.HandleAsync getReq

type internal AgentWrapperSpec<'args, 'model, 'msg, 'req, 'evt> when 'model : not struct and 'msg :> IMsg and 'req :> IReq and 'evt :> IEvt =
    WrapperSpec<IAgent<'args, 'model, 'msg, 'req, 'evt>, AgentModel<'args, 'model, 'msg, 'req, 'evt>, AgentMsg<'args, 'model, 'msg, 'req, 'evt>, NoModel, 'msg>

type internal AgentLogic<'args, 'model, 'msg, 'req, 'evt> when 'msg :> IMsg and 'req :> IReq and 'evt :> IEvt =
    Logic<IAgent<'args, 'model, 'msg, 'req, 'evt>,
            IAgent<'args, 'model, 'msg, 'req, 'evt>,
            NoArgs,
            AgentModel<'args, 'model, 'msg, 'req, 'evt>,
            AgentMsg<'args, 'model, 'msg, 'req, 'evt>>

and [<StructuredFormatDisplay("<Agent>{AsDisplay}")>]
    internal Agent<'args, 'model, 'msg, 'req, 'evt when 'model : not struct and 'msg :> IMsg and 'req :> IReq and 'evt :> IEvt> (spec', env', ident', logger') =
    let spec : AgentSpec<'args, 'model, 'msg, 'req, 'evt> = spec'
    let env : IEnv = env'
    let ident : Ident = ident'
    let mutable logger : ILogger = logger'
    let stats : Stats = statsOfCap <| defaultArg spec.GetSlowCap getDefaultSlowCap
    let taskManager : ITaskManager = new TaskManager () :> ITaskManager
    let mutable actor : Actor<'args, 'model, 'msg, 'req, 'evt> option = None
    let mutable logic : AgentLogic<'args, 'model, 'msg, 'req, 'evt> option = None
    let mutable disposed : bool = false
    let mutable dispatch : DispatchMsg<AgentMsg<'args, 'model, 'msg, 'req, 'evt>> option = None
    let mutable state : AgentModel<'args, 'model, 'msg, 'req, 'evt> option = None
    let mutable version : Version = Version.Init
    member _this.AsDisplay = (ident, version, actor)
    member this.AsAgent = this :> IAgent<'args, 'model, 'msg, 'req, 'evt>
    member this.SetupActor () =
        if actor.IsSome then
            raiseWithError "Agent" "Actor_Already_Setup" (actor)
        let args : 'args = spec.Actor.NewArgs (this :> IAgent)
        let (model, cmd) = spec.Actor.Logic.Init (this :> IAgent<'msg>) args
        let actor' = new Actor<'args, 'model, 'msg, 'req, 'evt> (this, spec.Actor, args, model)
        actor <- Some actor'
        let updateActor : Update<IAgent<'args, 'model, 'msg, 'req, 'evt>, NoModel, 'msg> =
            fun runner model' msg ->
                let (model, cmd) = spec.Actor.Logic.Update runner actor'.State msg
                actor'.SetState msg model
                (model', cmd)
        let wrapperSpec : AgentWrapperSpec<'args, 'model, 'msg, 'req, 'evt> =
            {
                GetSub = fun m -> NoModel
                SetSub = fun s -> id
                UpdateSub = updateActor
                ReactSub = noReaction
            }
        let wrapper = wrap ActorMsg' wrapperSpec
        (wrapper, model, cmd)
    member _this.SetLogic logic' =
        if logic.IsSome then
            raiseWithError "Agent" "Logic_Already_Set" (logic, logic')
        logic <- Some logic'
    member _this.Actor = actor |> Option.get
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
        member _this.Spec = spec
        member this.Actor = this.Actor :> IActor<'args, 'model, 'req, 'evt>

        member this.RunFunc3 func = runFunc' this func
        member this.AddTask3 onFailed getTask = addTask' this onFailed getTask
        member this.RunTask3 onFailed getTask = runTask' this onFailed getTask
module Dap.Platform.Internal.Env

open Dap.Prelude
open Dap.Context
open Dap.Platform

type internal EnvMsg =
    | EnvReq of EnvReq
    | EnvEvt of EnvEvt
with
    interface IMsg

type internal EnvOperate =
        Operate<IEnv, EnvModel, EnvMsg>

type internal EnvLogic =
    Logic<IEnv, IEnv, NoArgs, EnvModel, EnvMsg>

[<StructuredFormatDisplay("<Env>{AsDisplay}")>]
type internal Env (param' : EnvParam, logic') as this =
    let param : EnvParam = param'
    let logger : ILogger = param.Logging.GetLogger param.Scope
    let logic : EnvLogic = logic'
    let console : EnvConsole = new EnvConsole (this)
    let taskManager : ITaskManager = new TaskManager () :> ITaskManager
    let mutable dispatch : DispatchMsg<EnvMsg> option = None
    let mutable state : EnvModel option = None
    let mutable version : Version = Version.Init
    member __.AsDisplay = (param.Scope, version)
    member __.SetState (newState : EnvModel) =
        let stateChanged = state.IsNone || not (newState =? Option.get state)
        state <- Some newState
        version <- version.IncMsg stateChanged
    member __.Handle (req : EnvReq) = dispatch' this (EnvReq req)
    member __.HandleAsync (getReq : Callback<'res> -> EnvReq) = dispatchAsync' this (EnvReq << getReq)
    interface IRunnable<IEnv, IEnv, NoArgs, EnvModel, EnvMsg> with
        member __.Args = NoArgs
        member __.Logic = logic
        member __.Dispatch = dispatch
        member __.State = state
        member __.SetDispatch dispatch' = dispatch <- Some dispatch'
        member __.Start () = start' this this.SetState
        member __.Process parcel = process' this parcel this.SetState
        member __.Deliver msg = deliver' this msg
        member __.Initer = this :> IEnv
        member __.Runner = this :> IEnv
    interface IRunner with
        member __.Clock = param.Clock
        member __.Console0 = console :> IConsole
        member __.RunFunc0 func = runFunc' this func
        member __.AddTask0 onFailed getTask = addTask' this onFailed getTask
        member __.RunTask0 onFailed getTask = runTask' this onFailed getTask
    interface ITaskManager with
        member __.StartTask task = taskManager.StartTask task
        member __.ScheduleTask task = taskManager.ScheduleTask task
        member __.PendingTasksCount = taskManager.PendingTasksCount
        member __.StartPendingTasks () = taskManager.StartPendingTasks ()
        member __.ClearPendingTasks () = taskManager.ClearPendingTasks ()
        member __.RunningTasksCount = taskManager.RunningTasksCount
        member __.CancelRunningTasks () = taskManager.CancelRunningTasks ()
    interface IEnv with
        member __.Handle req = this.Handle req
        member __.HandleAsync getReq = this.HandleAsync getReq
        member __.Platform = param.Platform
        member __.Console = console
        member __.Logging = param.Logging
        member __.Scope = param.Scope
        member __.State = state |> Option.get
    interface IAspect with
        member __.Owner = this :> IOwner
    interface IOwner with
        member __.Luid = param.Scope
        member __.Disposed = false
    interface ILogger with
        member __.Log m = logger.Log m
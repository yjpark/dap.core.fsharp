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
type internal Env (param' : EnvParam, logic') =
    let param : EnvParam = param'
    let logger : ILogger = param.Logging.GetLogger param.Scope
    let logic : EnvLogic = logic'
    let stats : Stats = statsOfCap <| defaultArg param.GetSlowCap getDefaultSlowCap
    let taskManager : ITaskManager = new TaskManager () :> ITaskManager
    let mutable dispatch : DispatchMsg<EnvMsg> option = None
    let mutable state : EnvModel option = None
    let mutable version : Version = Version.Init
    member __.AsDisplay = (param.Scope, version)
    member this.SetState (newState : EnvModel) =
        let stateChanged = state.IsNone || not (newState =? Option.get state)
        state <- Some newState
        version <- version.IncMsg stateChanged
    member this.Handle (req : EnvReq) = dispatch' this (EnvReq req)
    member this.HandleAsync (getReq : Callback<'res> -> EnvReq) = dispatchAsync' this (EnvReq << getReq)
    interface IRunnable<IEnv, IEnv, NoArgs, EnvModel, EnvMsg> with
        member __.Args = NoArgs
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
        member this.Clock = param.Clock
        member this.Stats = stats
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
    interface IEnv with
        member this.Handle req = this.Handle req
        member this.HandleAsync getReq = this.HandleAsync getReq
        member this.Platform = param.Platform
        member this.Logging = param.Logging
        member this.Scope = param.Scope
        member this.State = state |> Option.get
    interface IOwner with
        member __.Luid = param.Scope
        member __.Disposed = false
    interface ILogger with
        member this.Log m = logger.Log m
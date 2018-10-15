[<AutoOpen>]
module Dap.Platform.BaseAgent

open Dap.Prelude
open Dap.Context
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
        (param : AgentParam) as this =
    let env : IEnv = param.Env
    let ident : Ident = Ident.Create param.Env.Scope param.Kind param.Key
    let mutable logger : ILogger = env.Logging.GetLogger <| ident.ToLuid ()
    let mutable spec : ActorSpec<'runner, 'args, 'model, 'msg, 'req, 'evt> option = None
    let mutable logic : AgentLogic<'runner, 'args, 'model, 'msg, 'req, 'evt> option = None
    let mutable console : AgentConsole = new AgentConsole (this, param, ident)
    let taskManager : ITaskManager = new TaskManager () :> ITaskManager
    let mutable disposed : bool = false
    let mutable dispatch : DispatchMsg<AgentMsg<'runner, 'args, 'model, 'msg, 'req, 'evt>> option = None
    let mutable state : AgentModel<'runner, 'args, 'model, 'msg, 'req, 'evt> option = None
    let mutable version : Version = Version.Init
    let onEvent = new Bus<AgentEvt> (this :> IOwner, "OnEvent")
    member this.AsDisplay = (ident, version, this.Actor)
    member this.AsAgent1 = this :> IAgent
    member this.AsAgent2 = this :> IAgent<'req, 'evt>
    member this.AsAgent2' = this :> IAgent<'msg>
    member this.AsAgent3 = this :> IAgent<'args, 'model, 'msg, 'req, 'evt>
    member this.AsAgent = this.AsAgent3
    member __.Setup' spec' logic' =
        if spec.IsSome then
            failWith "Already_Setup" (spec, spec')
        spec <- Some spec'
        logic <- Some logic'
    member __.SetState (newState : AgentModel<'runner, 'args, 'model, 'msg, 'req, 'evt>) =
        let stateChanged = state.IsNone || not (newState =? Option.get state)
        state <- Some newState
        version <- version.IncMsg stateChanged

    interface IRunnable<'runner, 'runner, NoArgs,
                        AgentModel<'runner, 'args, 'model, 'msg, 'req, 'evt>,
                        AgentMsg<'runner, 'args, 'model, 'msg, 'req, 'evt>> with
        member __.Args = NoArgs
        member __.Logic = logic |> Option.get
        member __.Dispatch = dispatch
        member __.State = state
        member __.SetDispatch dispatch' = dispatch <- Some dispatch'
        member this.Start () =
            let stateWasNone = state.IsNone
            let cmd = start' this this.SetState
            if stateWasNone then
                logger <- enrichLoggerForAgent this logger
            cmd
        member this.Process parcel = process' this parcel this.SetState
        member this.Deliver cmd = deliver' this cmd
        member this.Initer = this.Runner
        member this.Runner = this.Runner
    //IRunner
    member this.Clock = env.Clock
    member this.Console = console
    interface IRunner with
        member this.Clock = this.Clock
        member this.Console0 = this.Console :> IConsole
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
    member __.Log m = logger.Log m
    interface ILogger with
        member this.Log m = this.Log m
    interface IOwner with
        member __.Luid = ident.ToLuid ()
        member __.Disposed = disposed
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
    member __.Actor = state |> Option.get |> fun m -> m.Actor
    interface IAgent<'args, 'model, 'msg, 'req, 'evt> with
        member this.Actor = this.Actor

        member this.RunFunc3 func = runFunc' this func
        member this.AddTask3 onFailed getTask = addTask' this onFailed getTask
        member this.RunTask3 onFailed getTask = runTask' this onFailed getTask
        member this.AsAgent2 = this.AsAgent2
        member this.AsAgent2' = this.AsAgent2'
    //IAgent<'req, 'evt>
    member this.Post (subReq : 'req) = this.Actor.Handle subReq
    member this.PostAsync (getSubReq : Callback<'res> -> 'req) = this.Actor.HandleAsync getSubReq
    interface IAgent<'req, 'evt> with
        member this.Actor2 = this.Actor :> IActor<'req, 'evt>
        member this.Post subReq = this.Post subReq
        member this.PostAsync getSubReq = this.PostAsync getSubReq
        member this.RunFunc2 func = runFunc' this func
        member this.AddTask2 onFailed getTask = addTask' this onFailed getTask
        member this.RunTask2 onFailed getTask = runTask' this onFailed getTask
        member this.AsAgent1 = this.AsAgent1
    //IAgent<'msg>
    member this.Deliver (msg : 'msg) = dispatch' this (ActorMsg msg)
    member this.DeliverAsync (getMsg : Callback<'res> -> 'msg) = dispatchAsync' this (ActorMsg << getMsg)
    interface IAgent<'msg> with
        member this.Deliver msg = this.Deliver msg
        member this.DeliverAsync getMsg = this.DeliverAsync getMsg
    //IAgent
    member __.Env = env
    member __.Ident = ident
    member this.Handle (req : AgentReq) =
        version <- version.IncReq
        dispatch' this (AgentReq req)
    member this.HandleAsync (getReq : Callback<'res> -> AgentReq) =
        version <- version.IncReq
        dispatchAsync' this (AgentReq << getReq)
    member this.OnEvent = onEvent.Publish
    interface IAgent with
        member this.Env = this.Env
        member this.Ident = this.Ident
        member this.Handle req = this.Handle req
        member this.HandleAsync getReq = this.HandleAsync getReq
        member this.OnEvent = this.OnEvent
        member this.Console = this.Console
        member this.RunFunc1 func = runFunc' this func
        member this.AddTask1 onFailed getTask = addTask' this onFailed getTask
        member this.RunTask1 onFailed getTask = runTask' this onFailed getTask

[<AbstractClass>]
type PackAgent<'pack, 'runner, 'args, 'model, 'msg, 'req, 'evt
            when 'pack :> IPack
                    and 'runner :> IAgent<'args, 'model, 'msg, 'req, 'evt>
                    and 'model : not struct and 'msg :> IMsg
                    and 'req :> IReq and 'evt :> IEvt>
        (pack : 'pack, param) =
    inherit BaseAgent<'runner, 'args, 'model, 'msg, 'req, 'evt> (param)
    member __.Pack = pack
    interface IPackAgent<'pack> with
        member this.Pack = this.Pack

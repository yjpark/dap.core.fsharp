namespace Dap.Platform.Internal

open Elmish
open Dap.Prelude
open Dap.Platform

type internal EnvLogic =
    Logic<IEnv, IEnv, NoArgs, EnvModel, EnvMsg>

[<StructuredFormatDisplay("<Env>{Scope}")>]
type internal Env = {
    Platform : IPlatform
    Logging : ILogging
    Scope : Scope
    Clock : IClock
    Logger : ILogger
    Logic : EnvLogic
    Stats : Stats
    TaskManager : TaskManager
    mutable Dispatch : DispatchMsg<EnvMsg> option
    mutable State : EnvModel option
} with
    member this.SetState (state : EnvModel) = this.State <- Some state
    member this.Handle (req : EnvReq) = dispatch' this (EnvReq req)
    member this.HandleAsync (getReq : Callback<'res> -> EnvReq) = dispatchAsync' this (EnvReq << getReq)
    interface IRunnable<IEnv, IEnv, NoArgs, EnvModel, EnvMsg> with
        member _this.Args = NoArgs
        member this.Logic = this.Logic
        member this.Dispatch = this.Dispatch
        member this.State = this.State
        member this.SetDispatch dispatch = this.Dispatch <- Some dispatch
        member this.Start () = start' this this.SetState
        member this.Process parcel = process' this parcel this.SetState
        member this.Deliver msg = deliver' this msg
        member this.Initer = this :> IEnv
        member this.Runner = this :> IEnv
    interface IRunner with
        member this.Log m = this.Logger.Log m
        member this.Clock = this.Clock
        member this.Stats = this.Stats
        member this.RunFunc func = runFunc' this func
        member this.AddTask onFailed getTask = addTask' this onFailed getTask
        member this.ScheduleTask task = this.TaskManager.ScheduleTask task
        member this.RunTasks () = this.TaskManager.RunTasks ()
        member this.ClearPendingTasks () = this.TaskManager.ClearPendingTasks ()
        member this.CancelRunningTasks () = this.TaskManager.CancelRunningTasks ()
        member this.PendingTasksCount = this.TaskManager.PendingTasksCount
        member this.RunningTasksCount = this.TaskManager.RunningTasksCount
    interface IEnv with
        member this.Platform = this.Platform
        member this.Logging = this.Logging
        member this.Scope = this.Scope
        member this.State = this.State
        member this.Handle req = this.Handle req
        member this.HandleAsync getReq = this.HandleAsync getReq

type internal AgentLogic<'args, 'model, 'msg, 'req, 'evt> when 'msg :> IMsg and 'req :> IReq and 'evt :> IEvt =
    Logic<IAgent<'args, 'model, 'msg, 'req, 'evt>,
            IAgent<'args, 'model, 'msg, 'req, 'evt>,
            NoArgs,
            AgentModel<'args, 'model, 'msg, 'req, 'evt>,
            AgentMsg<'args, 'model, 'msg, 'req, 'evt>>

and [<StructuredFormatDisplay("<Agent>{AsDisplay}")>]
    internal Agent<'args, 'model, 'msg, 'req, 'evt>
                                    when 'msg :> IMsg and 'req :> IReq and 'evt :> IEvt = {
    Spec : AgentSpec<'args, 'model, 'msg, 'req, 'evt>
    Env : IEnv
    Ident : Ident
    mutable Logger : ILogger
    Logic : AgentLogic<'args, 'model, 'msg, 'req, 'evt>
    Stats : Stats
    TaskManager : TaskManager
    mutable Disposed : bool
    mutable Dispatch : DispatchMsg<AgentMsg<'args, 'model, 'msg, 'req, 'evt>> option
    mutable State : AgentModel<'args, 'model, 'msg, 'req, 'evt> option
    mutable Actor' : Actor<'args, 'model, 'msg, 'req, 'evt> option
    mutable Version : ActorVersion
} with
    member this.AsDisplay = (this.Ident, this.Version)
    member this.AsAgent = this :> IAgent<'args, 'model, 'msg, 'req, 'evt>
    member this.Actor =
        if this.Actor'.IsNone then
            this.Actor' <- Some <| Actor<'args, 'model, 'msg, 'req, 'evt>.Create this
        this.Actor'
        |> Option.get
    member this.SetState (state : AgentModel<'args, 'model, 'msg, 'req, 'evt>) =
        if this.State.IsSome && (state =? Option.get this.State) then
            this.Version <-
                {this.Version with
                    MsgCount = this.Version.MsgCount + 1
                }
        else
            this.Version <-
                {this.Version with
                    StateVer = this.Version.StateVer + 1
                    MsgCount = this.Version.MsgCount + 1
                }
            this.State <- Some state
    member this.Handle (req : AgentReq) =
        dispatch' this (AgentReq req)
    member this.HandleAsync (getReq : Callback<'res> -> AgentReq) =
        dispatchAsync' this (AgentReq << getReq)
    member this.Post (subReq : 'req) = dispatch' this (ActorMsg <| this.Spec.Actor.WrapReq subReq)
    member this.PostAsync (getSubReq : Callback<'res> -> 'req) = dispatchAsync' this (ActorMsg  << this.Spec.Actor.WrapReq << getSubReq)
    interface IRunnable<IAgent<'args, 'model, 'msg, 'req, 'evt>,
                        IAgent<'args, 'model, 'msg, 'req, 'evt>,
                        NoArgs,
                        AgentModel<'args, 'model, 'msg, 'req, 'evt>,
                        AgentMsg<'args, 'model, 'msg, 'req, 'evt>> with
        member _this.Args = NoArgs
        member this.Logic = this.Logic
        member this.Dispatch = this.Dispatch
        member this.State = this.State
        member this.SetDispatch dispatch = this.Dispatch <- Some dispatch
        member this.Start () =
            if this.State.IsNone then
                this.Logger <- enrichLoggerForAgent this this.Logger
            start' this this.SetState
        member this.Process parcel = process' this parcel this.SetState

        member this.Deliver cmd = deliver' this cmd
        member this.Initer = this.AsAgent
        member this.Runner = this.AsAgent
    interface IRunner with
        member this.Clock = this.Env.Clock
        member this.Stats = this.Stats
        member this.RunFunc func = runFunc' this func
        member this.AddTask onFailed getTask = addTask' this onFailed getTask
        member this.ScheduleTask task = this.TaskManager.ScheduleTask task
        member this.RunTasks () = this.TaskManager.RunTasks ()
        member this.ClearPendingTasks () = this.TaskManager.ClearPendingTasks ()
        member this.CancelRunningTasks () = this.TaskManager.CancelRunningTasks ()
        member this.PendingTasksCount = this.TaskManager.PendingTasksCount
        member this.RunningTasksCount = this.TaskManager.RunningTasksCount
    interface ILogger with
        member this.Log m = this.Logger.Log m
    interface IOwner with
        member this.Ident = this.Ident.Ident
        member this.Disposed = this.Disposed
    interface IAgent with
        member this.Env = this.Env
        member this.Ident = this.Ident
        member this.Handle req = this.Handle req
        member this.HandleAsync getReq = this.HandleAsync getReq
        member this.RunFunc1 func = runFunc' this func
        member this.AddTask1 onFailed getTask = addTask' this onFailed getTask
    interface IAgent<'req, 'evt> with
        member this.Actor = this.Actor :> IActor<'req, 'evt>
        member this.Post req = this.Post req
        member this.PostAsync getSubMsg = this.PostAsync getSubMsg
        member this.RunFunc2 func = runFunc' this func
        member this.AddTask2 onFailed getTask = addTask' this onFailed getTask
    interface IAgent<'msg> with
        member this.Deliver' (cmd : Cmd<'msg>) = deliver' this <| Cmd.map ActorMsg cmd
        member this.Deliver (msg : 'msg) = deliver' this <| Cmd.ofMsg (ActorMsg msg)
    interface IAgent<'args, 'model, 'msg, 'req, 'evt> with
        member this.Spec = this.Spec
        member this.Actor = this.Actor :> IActor<'args, 'model, 'req, 'evt>

        member this.RunFunc3 func = runFunc' this func
        member this.AddTask3 onFailed getTask = addTask' this onFailed getTask
        member this.FireEvent'' (evt : 'evt) = this.Actor.FireEvent' evt

and [<StructuredFormatDisplay("<Actor>{Ident}")>]
    internal Actor<'args, 'model, 'msg, 'req, 'evt>
                                    when 'msg :> IMsg and 'req :> IReq and 'evt :> IEvt = {
    Agent : Agent<'args, 'model, 'msg, 'req, 'evt>
    Args : 'args
    OnEvent : IBus<'evt>
    FireEvent' : 'evt -> unit
} with
    static member Create agent =
        let args = agent.Spec.Actor.NewArgs (agent :> IAgent)
        let event = new Bus<'evt> (agent :> IOwner)
        {
            Agent = agent
            Args = args
            OnEvent = event.Publish
            FireEvent' = event.Trigger
        }
    member this.Ident = this.Agent.Ident
    interface IActor<'args, 'model, 'req, 'evt> with
        member this.Handle req = this.Agent.Post req
        member this.OnEvent = this.OnEvent
        member this.Ident = this.Agent.Ident
        member this.Args = this.Args
        member this.State =
            this.Agent.State
            |> Option.map (fun s -> s.Actor)
            |> Option.get
        member this.Version =
            this.Agent.Version
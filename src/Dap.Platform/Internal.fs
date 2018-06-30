namespace Dap.Platform.Internal

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
        member this.RunTask onFailed getTask = runTask' this onFailed getTask
    interface IEnv with
        member this.Platform = this.Platform
        member this.Logging = this.Logging
        member this.Scope = this.Scope
        member this.State = this.State
        member this.Handle req = this.Handle req
        member this.HandleAsync getReq = this.HandleAsync getReq

type internal AgentLogic<'args, 'model, 'msg, 'req, 'evt> when 'msg :> IMsg and 'req :> IReq and 'evt :> IEvt =
    Logic<IAgent<'args, 'model, 'req, 'evt>,
            IAgent<'args, 'model, 'req, 'evt>,
            NoArgs,
            AgentModel<'args, 'model, 'msg, 'req, 'evt>,
            AgentMsg<'args, 'model, 'msg, 'req, 'evt>>

[<StructuredFormatDisplay("<Agent>{AsDisplay}")>]
type internal Agent<'args, 'model, 'msg, 'req, 'evt>
                                    when 'msg :> IMsg and 'req :> IReq and 'evt :> IEvt = {
    Spec : AgentSpec<'args, 'model, 'msg, 'req, 'evt>
    Env : IEnv
    Ident : Ident
    mutable Logger : ILogger
    Logic : AgentLogic<'args, 'model, 'msg, 'req, 'evt>
    Stats : Stats
    mutable Disposed : bool
    mutable Dispatch : DispatchMsg<AgentMsg<'args, 'model, 'msg, 'req, 'evt>> option
    mutable State : AgentModel<'args, 'model, 'msg, 'req, 'evt> option
    mutable Actor : Actor<'args, 'model, 'msg, 'req, 'evt> option
    mutable Version : ActorVersion
} with
    member this.AsDisplay = (this.Ident, this.Version)
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
    member this.Handle (req : AgentReq) = dispatch' this (AgentReq req)
    member this.HandleAsync (getReq : Callback<'res> -> AgentReq) =
        dispatchAsync' this (AgentReq << getReq)
    member this.Post (subReq : 'req) = dispatch' this (ActorMsg <| this.Spec.Actor.WrapReq subReq)
    member this.PostAsync (getSubReq : Callback<'res> -> 'req) = dispatchAsync' this (ActorMsg  << this.Spec.Actor.WrapReq << getSubReq)
    member this.EnsureActor =
        if this.Actor.IsNone then
            this.Actor <- Some <| Actor<'args, 'model, 'msg, 'req, 'evt>.Create this
        this.Actor
        |> Option.get
    interface IRunnable<IAgent<'args, 'model, 'req, 'evt>,
                        IAgent<'args, 'model, 'req, 'evt>,
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

        member this.Deliver msg = deliver' this msg
        member this.Initer = this :> IAgent<'args, 'model, 'req, 'evt>
        member this.Runner = this :> IAgent<'args, 'model, 'req, 'evt>
    interface IRunner with
        member this.Clock = this.Env.Clock
        member this.Stats = this.Stats
        member this.RunFunc func = runFunc' this func
        member this.RunTask onFailed getTask = runTask' this onFailed getTask
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
        member this.RunTask1 onFailed getTask = runTask' this onFailed getTask
    interface IAgent<'req, 'evt> with
        member this.Post req = this.Post req
        member this.PostAsync getSubMsg = this.PostAsync getSubMsg
        member this.RunFunc2 func = runFunc' this func
        member this.RunTask2 onFailed getTask = runTask' this onFailed getTask
        member this.Actor = this.EnsureActor :> IActor<'req, 'evt>
    interface IAgent<'args, 'model, 'req, 'evt> with
        member this.RunFunc3 func = runFunc' this func
        //Note: `((onFailed, getTask))` is needed, for the replyAsync type constraint to work
        member this.RunTask3 onFailed getTask = runTask' this onFailed getTask
        member this.Actor = this.EnsureActor :> IActor<'args, 'model, 'req, 'evt>

and [<StructuredFormatDisplay("<Actor>{Ident}")>]
    internal Actor<'args, 'model, 'msg, 'req, 'evt>
                                    when 'msg :> IMsg and 'req :> IReq and 'evt :> IEvt = {
    Agent : Agent<'args, 'model, 'msg, 'req, 'evt>
    Args : 'args
    OnEvent : IBus<'evt>
} with
    static member Create agent =
        let args = agent.Spec.Actor.NewArgs (agent :> IOwner)
        {
            Agent = agent
            Args = args
            OnEvent = agent.Spec.Actor.GetOnEvent args
        }
    member this.Ident = this.Agent.Ident
    interface IActor<'args, 'model, 'req, 'evt> with
        member this.Log m = this.Agent.Logger.Log m
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
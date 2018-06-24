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

type internal AgentLogic<'args, 'model, 'msg, 'req, 'evt> =
    Logic<IAgent<'req, 'evt>, IAgent<'model, 'req, 'evt>,
            NoArgs,
            AgentModel<'args, 'model, 'msg, 'req, 'evt>,
            AgentMsg<'args, 'model, 'msg, 'req, 'evt>>

[<StructuredFormatDisplay("<Agent>{Ident}")>]
type internal Agent<'args, 'model, 'msg, 'req, 'evt>
                                    when 'msg :> IMsg = {
    Spec : AgentSpec<'args, 'model, 'msg, 'req, 'evt>
    Env : IEnv
    Ident : Ident
    Logger : ILogger
    Logic : AgentLogic<'args, 'model, 'msg, 'req, 'evt>
    Stats : Stats
    mutable Disposed : bool
    mutable Dispatch : DispatchMsg<AgentMsg<'args, 'model, 'msg, 'req, 'evt>> option
    mutable State : AgentModel<'args, 'model, 'msg, 'req, 'evt> option
    mutable Actor : Actor<'args, 'model, 'msg, 'req, 'evt> option
} with
    member this.SetState (state : AgentModel<'args, 'model, 'msg, 'req, 'evt>) = this.State <- Some state
    member this.Handle (req : AgentReq) = dispatch' this (AgentReq req)
    member this.HandleAsync (getReq : Callback<'res> -> AgentReq) =
        dispatchAsync' this (AgentReq << getReq)
    member this.Post (subReq : 'req) = dispatch' this (ActorMsg <| this.Spec.Actor.WrapReq subReq)
    member this.PostAsync (getSubReq : Callback<'res> -> 'req) = dispatchAsync' this (ActorMsg  << this.Spec.Actor.WrapReq << getSubReq)
    interface IRunnable<IAgent<'req, 'evt>,
                        IAgent<'model, 'req, 'evt>,
                        NoArgs,
                        AgentModel<'args, 'model, 'msg, 'req, 'evt>,
                        AgentMsg<'args, 'model, 'msg, 'req, 'evt>> with
        member _this.Args = NoArgs
        member this.Logic = this.Logic
        member this.Dispatch = this.Dispatch
        member this.State = this.State
        member this.SetDispatch dispatch = this.Dispatch <- Some dispatch
        member this.Start () = start' this this.SetState
        member this.Process parcel = process' this parcel this.SetState

        member this.Deliver msg = deliver' this msg
        member this.Initer = this :> IAgent<'req, 'evt>
        member this.Runner = this :> IAgent<'model, 'req, 'evt>
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
        member this.Actor =
            match this.Actor with
            | Some actor ->
                actor :> IActor<'req, 'evt>
            | None ->
                let actor = {
                    Agent = this
                }
                this.Actor <- Some actor
                actor :> IActor<'req, 'evt>
    interface IAgent<'model, 'req, 'evt> with
        member this.RunFunc3 func = runFunc' this func
        //Note: `((onFailed, getTask))` is needed, for the replyAsync type constraint to work
        member this.RunTask3 onFailed getTask = runTask' this onFailed getTask
        member this.Actor =
            match this.Actor with
            | Some actor ->
                actor :> IActor<'model, 'req, 'evt>
            | None ->
                let actor = {
                    Agent = this
                }
                this.Actor <- Some actor
                actor :> IActor<'model, 'req, 'evt>

and [<StructuredFormatDisplay("<Actor>{Ident}")>]
    internal Actor<'args, 'model, 'msg, 'req, 'evt>
                                    when 'msg :> IMsg = {
    Agent : Agent<'args, 'model, 'msg, 'req, 'evt>
} with
    member this.Ident = this.Agent.Ident
    interface IActor<'model, 'req, 'evt> with
        member this.Log m = this.Agent.Logger.Log m
        member this.Handle req = this.Agent.Post req
        member this.OnEvent =
            let model = Option.get this.Agent.State
            this.Agent.Spec.Actor.GetOnEvent model.Actor
        member this.Ident = this.Agent.Ident
        member this.State =
            this.Agent.State
            |> Option.map (fun s -> s.Actor)
            |> Option.get
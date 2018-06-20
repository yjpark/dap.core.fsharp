namespace Dap.Platform.Internal

open Dap.Prelude
open Dap.Platform

[<StructuredFormatDisplay("{AsString}")>]
type internal Env = {
    Platform : IPlatform
    Logging : ILogging
    Scope : Scope
    Clock : IClock
    Logger : ILogger
    Logic : Logic<IEnv, NoArgs, EnvModel, EnvMsg>
    Stats : Stats
    mutable Dispatch : DispatchMsg<EnvMsg> option
    mutable State : EnvModel option
} with
    override this.ToString () = sprintf "[<Env>%s]" this.Scope
    member this.AsString = this.ToString ()
    member this.SetState (state : EnvModel) = this.State <- Some state
    member this.Handle (req : EnvReq) = dispatch' this (EnvReq req)
    member this.HandleAsync (getReq : Callback<'res> -> EnvReq) = dispatchAsync' this (EnvReq << getReq)
    interface IRunnable<IEnv, NoArgs, EnvModel, EnvMsg> with
        member _this.Args = NoArgs
        member this.Logic = this.Logic
        member this.Dispatch = this.Dispatch
        member this.State = this.State
        member this.SetDispatch dispatch = this.Dispatch <- Some dispatch
        member this.Start () = start' this this.SetState
        member this.Process parcel = process' this parcel this.SetState
        member this.Deliver msg = deliver' this msg
    interface IRunner<IEnv> with
        member this.Self = this :> IEnv
        member this.RunFunc' func = runFunc'' this func
        member this.RunTask' onFailed getTask = runTask'' this onFailed getTask
        //member this.AwaitTask' getTask = awaitTask'' this getTask
    interface IRunner with
        member this.Log m = this.Logger.Log m
        member this.Clock = this.Clock
        member this.Stats = this.Stats
        member this.RunFunc func = runFunc' this func
        member this.RunTask onFailed getTask = runTask' this onFailed getTask
        //member this.AwaitTask getTask = awaitTask' this getTask
    interface IEnv with
        member this.Platform = this.Platform
        member this.Logging = this.Logging
        member this.Scope = this.Scope
        member this.State = this.State
        member this.Handle req = this.Handle req
        member this.HandleAsync getReq = this.HandleAsync getReq

[<StructuredFormatDisplay("{AsString}")>]
type internal Agent<'args, 'model, 'msg, 'req, 'evt>
                                    when 'msg :> IMsg = {
    Spec : AgentSpec<'args, 'model, 'msg, 'req, 'evt>
    Env : IEnv
    Ident : Ident
    Logger : ILogger
    Logic : Logic<IAgent, NoArgs,
                    AgentModel<'args, 'model, 'msg, 'req, 'evt>,
                    AgentMsg<'args, 'model, 'msg, 'req, 'evt>>
    Stats : Stats
    mutable Dispatch : DispatchMsg<AgentMsg<'args, 'model, 'msg, 'req, 'evt>> option
    mutable State : AgentModel<'args, 'model, 'msg, 'req, 'evt> option
    mutable Actor : IActor<'model, 'req, 'evt> option
} with
    override this.ToString () = sprintf "[<Agent>%s.%s.%s]" this.Ident.Scope this.Ident.Kind this.Ident.Key
    member this.AsString = this.ToString ()
    member this.SetState (state : AgentModel<'args, 'model, 'msg, 'req, 'evt>) = this.State <- Some state
    member this.Handle (req : AgentReq) = dispatch' this (AgentReq req)
    member this.HandleAsync (getReq : Callback<'res> -> AgentReq) =
        dispatchAsync' this (AgentReq << getReq)
    member this.Post (subReq : 'req) = dispatch' this (ActorMsg <| this.Spec.Actor.WrapReq subReq)
    member this.PostAsync (getSubReq : Callback<'res> -> 'req) = dispatchAsync' this (ActorMsg  << this.Spec.Actor.WrapReq << getSubReq)
    interface IRunnable<IAgent, NoArgs,
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
    interface IRunner<IAgent> with
        member this.Self = this :> IAgent
        member this.RunFunc' getFunc = runFunc'' this getFunc
        member this.RunTask' onFailed getTask = runTask'' this onFailed getTask
        //member this.AwaitTask' getTask = awaitTask'' this getTask
    interface IRunner with
        member this.Log m = this.Logger.Log m
        member this.Clock = this.Env.Clock
        member this.Stats = this.Stats
        member this.RunFunc getFunc = runFunc' this getFunc
        member this.RunTask onFailed getTask = runTask' this onFailed getTask
        //member this.AwaitTask getTask = awaitTask' this getTask
    interface IAgent<'req, 'evt> with
        member this.Env = this.Env
        member this.Ident = this.Ident
        member this.Handle req = this.Handle req
        member this.HandleAsync getReq = this.HandleAsync getReq
        member this.Post req = this.Post req
        member this.PostAsync getSubMsg = this.PostAsync getSubMsg
        member this.Actor =
            match this.Actor with
            | Some actor ->
                actor :> IActor<'req, 'evt>
            | None ->
                let actor = {
                    Agent = this
                }
                let actor = actor :> IActor<'model, 'req, 'evt>
                this.Actor <- Some actor
                actor :> IActor<'req, 'evt>
    interface IAgent<'model, 'req, 'evt> with
        member this.Actor =
            match this.Actor with
            | Some actor ->
                actor
            | None ->
                let actor = {
                    Agent = this
                }
                let actor = actor :> IActor<'model, 'req, 'evt>
                this.Actor <- Some actor
                actor

and internal Proxy<'args, 'model, 'msg, 'req, 'evt>
                                    when 'msg :> IMsg = {
    Agent : Agent<'args, 'model, 'msg, 'req, 'evt>
} with
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
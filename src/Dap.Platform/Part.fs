[<AutoOpen>]
[<RequireQualifiedAccess>]
module Dap.Platform.Part

open FSharp.Control.Tasks.V2

open Dap.Prelude
open Dap.Context

type IPart =
    inherit IOwner
    abstract Agent : IAgent with get

type IPart<'actorMsg, 'args, 'model, 'msg, 'req, 'evt>
                when 'actorMsg :> IMsg
                and 'model : not struct and 'msg :> IMsg
                and 'req :> IReq and 'evt :> IEvt =
    inherit IPart
    inherit IAgent<'args, 'model, 'msg, 'req, 'evt>
    abstract Agent : IAgent<'actorMsg> with get
    abstract Part : IActor<'args, 'model, 'req, 'evt> with get
    abstract Wrapper : Wrapper<'actorMsg, 'msg> with get
    abstract RunFunc4<'res> : Func<IPart<'actorMsg, 'args, 'model, 'msg, 'req, 'evt>, 'res> -> Result<'res, exn>
    abstract AddTask4 : OnFailed<IPart<'actorMsg, 'args, 'model, 'msg, 'req, 'evt>> -> GetTask<IPart<'actorMsg, 'args, 'model, 'msg, 'req, 'evt>, unit> -> unit
    abstract RunTask4 : OnFailed<IPart<'actorMsg, 'args, 'model, 'msg, 'req, 'evt>> -> GetTask<IPart<'actorMsg, 'args, 'model, 'msg, 'req, 'evt>, unit> -> unit
    abstract Setup' : IAgent<'actorMsg> -> Wrapper<'actorMsg, 'msg> ->
                        IActor<'args, 'model, 'req, 'evt> -> Wrapper<'actorMsg, 'msg> ->
                        Cmd<'msg> -> unit

[<StructuredFormatDisplay("<Part>{AsDisplay}")>]
[<AbstractClass>]
type BasePart<'actorMsg, 'runner, 'args, 'model, 'msg, 'req, 'evt
            when 'actorMsg :> IMsg
                and 'runner :> IPart<'actorMsg, 'args, 'model, 'msg, 'req, 'evt>
                and 'model : not struct and 'msg :> IMsg
                and 'req :> IReq and 'evt :> IEvt>
        (param) =
    let env : IEnv = param.Env
    let ident : Ident = Ident.Create param.Env.Scope param.Kind param.Key
    let mutable agent : IAgent<'actorMsg> option = None
    let mutable partMsg : Wrapper<'actorMsg, 'msg> option = None
    let mutable actor : IActor<'args, 'model, 'req, 'evt> option = None
    let mutable wrapper : Wrapper<'actorMsg, 'msg> option = None
    let mutable logger : ILogger option = None
    let mutable dispatch : DispatchMsg<'msg> option = None
    member __.AsDisplay = (ident, partMsg, actor)
    member this.AsPart = this :> IPart<'actorMsg, 'args, 'model, 'msg, 'req, 'evt>
    member this.AsAgent = this :> IAgent<'args, 'model, 'msg, 'req, 'evt>
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
    //IPart
    member __.Agent = agent |> Option.get
    member __.Wrapper = wrapper |> Option.get
    interface IPart with
        member this.Agent = this.Agent :> IAgent
    member __.Part = actor |> Option.get
    interface IPart<'actorMsg, 'args, 'model, 'msg, 'req, 'evt> with
        member this.Agent = this.Agent
        member this.Part = this.Part
        member this.Wrapper = this.Wrapper
        member this.RunFunc4 func = runFunc' this func
        member this.AddTask4 onFailed getTask = addTask' this onFailed getTask
        member this.RunTask4 onFailed getTask = runTask' this onFailed getTask
        member this.Setup' agent' partMsg' actor' wrapper' cmd =
            if agent.IsSome then
                failWith "Already_Setup" (agent, agent')
            if env <> agent'.Env || ident <> agent'.Ident then
                failWith "Invalid_Agent" (env, ident, agent')
            agent <- Some agent'
            partMsg <- Some partMsg'
            actor <- Some actor'
            wrapper <- Some wrapper'
            wrapper <- Some <| wrapper'
            logger <- Some <| enrichLoggerForAgent this ^<| env.Logging.GetLogger ^<| sprintf "%s%A" (ident.ToLuid ()) partMsg'
            (this :> IDispatcher<'msg>).SetDispatch (fun (_time, msg) ->
                agent'.Deliver <| partMsg' msg
            )
            if cmd.Length > 0 then
                this.AddTask ignoreOnFailed (fun _ -> task {
                    cmd |> List.iter (fun m -> m <| dispatch' this)
                })
    interface IDispatcher<'msg> with
        member __.Dispatch = dispatch
        member __.SetDispatch dispatch' = dispatch <- Some dispatch'
    //ILogger
    member __.Log m = (logger |> Option.get).Log m
    interface ILogger with
        member this.Log m = this.Log m
    //IRunner
    member this.Clock = env.Clock
    interface IRunner with
        member this.Clock = this.Clock
        member this.Stats = this.Agent.Stats
        member this.RunFunc0 func = runFunc' this func
        member this.AddTask0 onFailed getTask = addTask' this onFailed getTask
        member this.RunTask0 onFailed getTask = runTask' this onFailed getTask
    interface ITaskManager with
        member this.StartTask task = this.Agent.StartTask task
        member this.ScheduleTask task = this.Agent.ScheduleTask task
        member this.PendingTasksCount = this.Agent.PendingTasksCount
        member this.StartPendingTasks () = this.Agent.StartPendingTasks ()
        member this.ClearPendingTasks () = this.Agent.ClearPendingTasks ()
        member this.RunningTasksCount = this.Agent.RunningTasksCount
        member this.CancelRunningTasks () = this.Agent.CancelRunningTasks ()
    member this.Owner = this.Agent :> IOwner
    interface IOwner with
        member this.Luid = this.Owner.Luid
        member this.Disposed = this.Owner.Disposed
    //IAgent<'args, 'model, 'msg, 'req, 'evt>
    interface IAgent<'args, 'model, 'msg, 'req, 'evt> with
        member this.Actor = this.Part
        member this.RunFunc3 func = runFunc' this func
        member this.AddTask3 onFailed getTask = addTask' this onFailed getTask
        member this.RunTask3 onFailed getTask = runTask' this onFailed getTask
        member this.AsAgent3 = this :> IAgent<'args, 'model, 'msg, 'req, 'evt>
    //IAgent<'req, 'evt>
    member this.Post (req : 'req) = this.Part.Handle req
    member this.PostAsync (getReq : Callback<'res> -> 'req) = this.Part.HandleAsync getReq
    interface IAgent<'req, 'evt> with
        member this.Post req = this.Post req
        member this.PostAsync getReq = this.PostAsync getReq
        member this.Actor = this.Part :> IActor<'req, 'evt>
        member this.RunFunc2 func = runFunc' this func
        member this.AddTask2 onFailed getTask = addTask' this onFailed getTask
        member this.RunTask2 onFailed getTask = runTask' this onFailed getTask
        member this.AsAgent2 = this :> IAgent<'req, 'evt>
    //IAgent<'msg>
    member this.Deliver (msg : 'msg) = dispatch' this msg
    member this.DeliverAsync (getMsg : Callback<'res> -> 'msg) = dispatchAsync' this getMsg
    interface IAgent<'msg> with
        member this.Deliver msg = this.Deliver msg
        member this.DeliverAsync getMsg = this.DeliverAsync getMsg
        member this.AsAgent2' = this :> IAgent<'msg>
    //IAgent
    member __.Env = env
    member __.Ident = ident
    member this.Handle (req : AgentReq) = this.Agent.Handle req
    member this.HandleAsync (getReq : Callback<'res> -> AgentReq) = this.Agent.HandleAsync getReq
    interface IAgent with
        member this.Env = this.Env
        member this.Ident = this.Ident
        member this.Handle req = this.Handle req
        member this.HandleAsync getReq = this.HandleAsync getReq
        member this.RunFunc1 func = runFunc' this func
        member this.AddTask1 onFailed getTask = addTask' this onFailed getTask
        member this.RunTask1 onFailed getTask = runTask' this onFailed getTask
        member this.AsAgent1 = this :> IAgent
    interface IAspect with
        member this.Owner = this.Agent :> IOwner

let create<'actorRunner, 'actorModel, 'actorMsg, 'runner, 'args, 'model, 'msg, 'req, 'evt
            when 'actorRunner :> IAgent<'actorMsg> and 'actorMsg :> IMsg
                and 'runner :> IPart<'actorMsg, 'args, 'model, 'msg, 'req, 'evt>
                and 'model : not struct and 'msg :> IMsg
                and 'req :> IReq and 'evt :> IEvt>
        (spec : ActorSpec<'runner, 'args, 'model, 'msg, 'req, 'evt>)
        (partMsg : Wrapper<'actorMsg, 'msg>)
        (wrapMsg : WrapMsg<'actorRunner, 'actorModel, 'actorMsg>)
        (agent : IAgent<'actorMsg>)
        : 'runner =
    let part = spec.Spawner <| AgentParam.Create agent.Env agent.Ident.Kind agent.Ident.Key
    let (actor, wrapper, cmd) = part |> createActor' spec wrapMsg true
    part.Setup' agent partMsg actor wrapper cmd
    part

let replyAsync4 (runner : IPart<'actorMsg, 'args, 'model, 'msg, 'req, 'evt>) (req : IReq) (callback : Callback<'res>)
                (getOnFailed: OnReplyFailed<IPart<'actorMsg, 'args, 'model, 'msg, 'req, 'evt>, 'res>)
                (getReplyTask : GetReplyTask<IPart<'actorMsg, 'args, 'model, 'msg, 'req, 'evt>, 'res>) : unit =
    let onFailed = getOnFailed req callback
    let getTask = getReplyTask req callback
    runner.AddTask4 onFailed getTask

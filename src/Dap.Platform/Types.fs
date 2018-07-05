[<AutoOpen>]
module Dap.Platform.Types'

open System.Threading.Tasks
open Dap.Prelude

type QuitStats = {
    ForceQuit : bool
    ProcessTime : Instant
    QuitDuration : Duration
}

type StopStats = {
    ForceStop : bool
    ProcessTime : Instant
    StopDuration : Duration
}

and GetReplyTask<'runner, 'res when 'runner :> IRunner> =
    IReq -> Callback<'res> -> GetTask<'runner, unit>
and OnReplyFailed<'runner, 'res when 'runner :> IRunner> =
    IReq -> Callback<'res> -> 'runner -> exn -> unit

type IPlatform =
    abstract Start : IRunnable<'initer, 'runner, 'args, 'model, 'msg> -> unit

and IEnv =
    inherit IRunner
    inherit IHandler<EnvReq>
    inherit IAsyncHandler<EnvReq>
    abstract Platform : IPlatform with get
    abstract Logging : ILogging with get
    abstract Scope : Scope with get
    abstract State : EnvModel option with get

and Spawner =
    Ident -> IAgent

and EnvModel = {
    Services : Map<Kind, Map<Key, IAgent>>
    Spawners : Map<Kind, Spawner>
    Agents : Map<Kind, Map<Key, IAgent>>
}

and EnvReq =
    | DoQuit of bool * Callback<QuitStats>                      // forceQuit -> willQuit
    | DoAddService of IAgent * Callback<int * int>              // -> servicesCount * kindServicesCount
    | DoGetService of Kind * Key * Callback<IAgent>             // -> service
    | TryFindService of Kind * Key * Callback<IAgent option>    // -> service
    | DoRegister of Kind * Spawner * Callback<int>              // -> spawnersCount
    | DoGetAgent of Kind * Key * Callback<IAgent * bool>        // -> (agent, isNew)
with interface IReq

and EnvEvt =
    | OnQuit of QuitStats
    | OnNewAgent of Kind * Key * IAgent
with interface IEvt

and EnvMsg =
    | EnvReq of EnvReq
    | EnvEvt of EnvEvt
with
    interface IMsg

and EnvOperate =
        Operate<IEnv, EnvModel, EnvMsg>

and EnvParam = {
    Platform : IPlatform
    Logging : ILogging
    Scope : Scope
    Clock : IClock
    GetSlowCap : GetSlowCap option
}

and IAgent =
    inherit IOwner
    inherit IRunner
    inherit IHandler<AgentReq>
    inherit IAsyncHandler<AgentReq>
    //inherit IChannel<AgentEvt>
    abstract Env : IEnv with get
    abstract Ident : Ident with get
    abstract RunFunc1<'res> : Func<IAgent, 'res> -> Result<'res, exn>
    abstract AddTask1 : OnFailed<IAgent> -> GetTask<IAgent, unit> -> unit
    abstract RunTask1 : OnFailed<IAgent> -> GetTask<IAgent, unit> -> unit

and IAgent<'req, 'evt> when 'req :> IReq and 'evt :> IEvt =
    inherit IAgent
    inherit IPoster<'req>
    inherit IAsyncPoster<'req>
    abstract Actor : IActor<'req, 'evt> with get
    abstract RunFunc2<'res> : Func<IAgent<'req, 'evt>, 'res> -> Result<'res, exn>
    abstract AddTask2 : OnFailed<IAgent<'req, 'evt>> -> GetTask<IAgent<'req, 'evt>, unit> -> unit
    abstract RunTask2 : OnFailed<IAgent<'req, 'evt>> -> GetTask<IAgent<'req, 'evt>, unit> -> unit

and IAgent<'msg> when 'msg :> IMsg =
    inherit IAgent
    abstract Deliver : 'msg -> unit
    abstract DeliverAsync<'res> : (Callback<'res> -> 'msg) -> Task<'res>

and IAgent<'args, 'model, 'msg, 'req, 'evt> when 'msg :> IMsg and 'req :> IReq and 'evt :> IEvt =
    inherit IAgent<'msg>
    inherit IAgent<'req, 'evt>
    abstract Spec : AgentSpec<'args, 'model, 'msg, 'req, 'evt> with get
    abstract Actor : IActor<'args, 'model, 'req, 'evt> with get
    abstract RunFunc3<'res> : Func<IAgent<'args, 'model, 'msg, 'req, 'evt>, 'res> -> Result<'res, exn>
    abstract AddTask3 : OnFailed<IAgent<'args, 'model, 'msg, 'req, 'evt>> -> GetTask<IAgent<'args, 'model, 'msg, 'req, 'evt>, unit> -> unit
    abstract RunTask3 : OnFailed<IAgent<'args, 'model, 'msg, 'req, 'evt>> -> GetTask<IAgent<'args, 'model, 'msg, 'req, 'evt>, unit> -> unit

and AgentSpec<'args, 'model, 'msg, 'req, 'evt> when 'msg :> IMsg and 'req :> IReq and 'evt :> IEvt = {
    Actor : ActorSpec'<IAgent<'msg>, IAgent<'args, 'model, 'msg, 'req, 'evt>, 'args, 'model, 'msg, 'req, 'evt>
    OnAgentEvent : OnAgentEvent<'model> option
    GetSlowCap : GetSlowCap option
}

and OnAgentEvent<'model> = IAgent -> 'model -> AgentEvt -> unit

and AgentParam = {
    Env : IEnv
    Kind : Kind
    Key : Key
}

and AgentModel<'args, 'model, 'msg, 'req, 'evt> when 'msg :> IMsg and 'req :> IReq and 'evt :> IEvt = {
    Spec : AgentSpec<'args, 'model, 'msg, 'req, 'evt>
}

and AgentReq =
    | DoStop of bool * Callback<StopStats>           // forceStop
with interface IReq

and AgentEvt =
    | OnWillStop of Callback<unit>
    | OnDidStop of StopStats
with interface IEvt

and AgentMsg<'args, 'model, 'msg, 'req, 'evt> when 'msg :> IMsg and 'req :> IReq and 'evt :> IEvt =
    | AgentReq of AgentReq
    | AgentEvt of AgentEvt
    | ActorMsg of 'msg
    | ActorMsg' of AgentWrapping<'args, 'model, 'msg, 'req, 'evt>
with interface IMsg

and AgentWrapping<'args, 'model, 'msg, 'req, 'evt> when 'msg :> IMsg and 'req :> IReq and 'evt :> IEvt =
    IWrapping<IAgent<'args, 'model, 'msg, 'req, 'evt>, AgentModel<'args, 'model, 'msg, 'req, 'evt>, AgentMsg<'args, 'model, 'msg, 'req, 'evt>>

let DoQuit' (forceQuit : bool) callback =
    DoQuit (forceQuit, callback)

let DoAddService' (service : IAgent) callback =
    DoAddService (service, callback)

let DoGetService' (kind : Kind) (key : Key) callback =
    DoGetService (kind, key, callback)

let TryFindService' (kind : Kind) (key : Key) callback =
    TryFindService (kind, key, callback)

let DoRegister' (kind : Kind) (spawner : Spawner) callback =
    DoRegister (kind, spawner, callback)

let DoGetAgent' (kind : Kind) (key : Key) callback =
    DoGetAgent (kind, key, callback)

let DoStop' (forceStop : bool) callback =
    DoStop (forceStop, callback)


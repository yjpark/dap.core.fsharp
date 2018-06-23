[<AutoOpen>]
module Dap.Platform.Types'

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

type IPlatform =
    abstract Start : IRunnable<'runner, 'args, 'model, 'msg> -> unit

type IEnv =
    inherit IRunner<IEnv>
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

and EnvEvt =
    | OnQuit of QuitStats
    | OnNewAgent of Kind * Key * IAgent

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
    inherit IRunner<IAgent>
    inherit IHandler<AgentReq>
    inherit IAsyncHandler<AgentReq>
    //inherit IChannel<AgentEvt>
    abstract Env : IEnv with get
    abstract Ident : Ident with get

and IAgent<'req, 'evt> =
    inherit IAgent
    inherit IPoster<'req>
    inherit IAsyncPoster<'req>
    abstract Actor : IActor<'req, 'evt> with get

and IAgent<'model, 'req, 'evt> =
    inherit IAgent<'req, 'evt>
    abstract Actor : IActor<'model, 'req, 'evt> with get

and IAsyncAgent<'model, 'req, 'evt> =
    inherit IHandler<'req>

and AgentSpec<'args, 'model, 'msg, 'req, 'evt> = {
    Actor : ActorSpec<IAgent, 'args, 'model, 'msg, 'req, 'evt>
    OnAgentEvent : OnAgentEvent<'model> option
    GetSlowCap : GetSlowCap option
}

and OnAgentEvent<'model> = IAgent -> 'model -> AgentEvt -> unit

and AgentParam = {
    Env : IEnv
    Kind : Kind
    Key : Key
}

and AgentModel<'args, 'model, 'msg, 'req, 'evt> = {
    Spec : AgentSpec<'args, 'model, 'msg, 'req, 'evt>
    Actor : 'model
}

and AgentReq =
    | DoStop of bool * Callback<StopStats>           // forceStop

and AgentEvt =
    | OnWillStop of Callback<unit> 
    | OnDidStop of StopStats

and AgentMsg<'args, 'model, 'msg, 'req, 'evt> =
    | AgentReq of AgentReq
    | AgentEvt of AgentEvt
    | ActorMsg of 'msg
    | ActorMsg' of AgentWrapping<'args, 'model, 'msg, 'req, 'evt>
with interface IMsg

and AgentOperate<'args, 'model, 'msg, 'req, 'evt> =
    Operate<IAgent, AgentModel<'args, 'model, 'msg, 'req, 'evt>, AgentMsg<'args, 'model, 'msg, 'req, 'evt>>

and AgentWrapping<'args, 'model, 'msg, 'req, 'evt> =
    IWrapping<IAgent, AgentModel<'args, 'model, 'msg, 'req, 'evt>, AgentMsg<'args, 'model, 'msg, 'req, 'evt>>

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


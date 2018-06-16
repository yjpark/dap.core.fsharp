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
    Services : Map<Kind, IAgent>
    Spawners : Map<Kind, Spawner>
    Agents : Map<Kind, Map<Key, IAgent>>
}

and EnvReq =
    | DoQuit of bool * Callback<QuitStats>                  // forceQuit -> willQuit
    | DoAddService of IAgent * Callback<int>                // -> servicesCount
    | DoGetService of Kind * Callback<IAgent>               // -> service
    | DoRegister of Kind * Spawner * Callback<int>          // -> spawnersCount
    | DoGetAgent of Kind * Key * Callback<IAgent * bool>    // -> (agent, isNew)

and EnvEvt =
    | OnQuit of QuitStats
    | OnNewAgent of Kind * Key * IAgent

and EnvMsg =
    | EnvReq of EnvReq
    | EnvEvt of EnvEvt
with interface IMsg

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
    | ActorMsg' of AgentOperate<'args, 'model, 'msg, 'req, 'evt>
with interface IMsg

and AgentOperate<'args, 'model, 'msg, 'req, 'evt> =
    Operate<IAgent,
            AgentModel<'args, 'model, 'msg, 'req, 'evt>,
            AgentMsg<'args, 'model, 'msg, 'req, 'evt>>

let identOf (scope : Scope) (kind : Kind) (key : Key) : Ident =
    {
        Scope = scope
        Kind = kind
        Key = key
    }
[<AutoOpen>]
module Dap.Platform.Types'

open System.Threading.Tasks
open Dap.Prelude
open Dap.Context

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

and EnvParam = {
    Platform : IPlatform
    Logging : ILogging
    Scope : Scope
    Clock : IClock
}

and EnvConsole (env : IEnv, param : EnvParam) =
    inherit Console<EnvConsole> (EnvConsoleKind, param.Logging, param.Clock)
    do (
        let scope = base.Stats.Target.AddVar<Scope> (E.string, D.string, "scope", param.Scope, None)
        scope.Seal ()
    )
    override this.Self = this
    override __.Spawn l = failWith "Invalid_Operation" "EnvConsole.Spawn"

and IEnv =
    inherit IOwner
    inherit IRunner
    abstract Handle : EnvReq -> unit
    abstract HandleAsync<'res> : (Callback<'res> -> EnvReq) -> Task<'res>
    abstract Platform : IPlatform with get
    abstract Console : EnvConsole with get
    abstract Logging : ILogging with get
    abstract Scope : Scope with get
    abstract State : EnvModel with get

and Spawner = AgentParam -> IAgent

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

and AgentParam = {
    Env : IEnv
    Kind : Kind
    Key : Key
} with
    static member Create env kind key =
        {
            Env = env
            Kind = kind
            Key = key
        }

and AgentConsole (agent : IAgent, param : AgentParam, ident : Ident) =
    inherit Console<AgentConsole> (AgentConsoleKind, param.Env.Logging, param.Env.Clock)
    do (
        let ident' = base.Stats.Target.AddVar<Ident> (E.ident, D.ident, "ident", ident, None)
        ident'.Seal ()
    )
    override this.Self = this
    override __.Spawn l = failWith "Invalid_Operation" "AgentConsole.Spawn"

and IAgent =
    inherit IOwner
    inherit IRunner
    abstract Handle : AgentReq -> unit
    abstract HandleAsync<'res> : (Callback<'res> -> AgentReq) -> Task<'res>
    abstract OnEvent : IBus<AgentEvt> with get
    abstract Env : IEnv with get
    abstract Ident : Ident with get
    abstract Console : AgentConsole with get
    abstract RunFunc1<'res> : Func<IAgent, 'res> -> Result<'res, exn>
    abstract AddTask1 : OnFailed<IAgent> -> GetTask<IAgent, unit> -> unit
    abstract RunTask1 : OnFailed<IAgent> -> GetTask<IAgent, unit> -> unit

and IAgent<'req, 'evt> when 'req :> IReq and 'evt :> IEvt =
    inherit IAgent
    inherit IPoster<'req>
    inherit IAsyncPoster<'req>
    abstract Actor2 : IActor<'req, 'evt> with get
    abstract RunFunc2<'res> : Func<IAgent<'req, 'evt>, 'res> -> Result<'res, exn>
    abstract AddTask2 : OnFailed<IAgent<'req, 'evt>> -> GetTask<IAgent<'req, 'evt>, unit> -> unit
    abstract RunTask2 : OnFailed<IAgent<'req, 'evt>> -> GetTask<IAgent<'req, 'evt>, unit> -> unit
    abstract AsAgent1 : IAgent with get

and IAgent<'msg> when 'msg :> IMsg =
    inherit IAgent
    abstract Deliver : 'msg -> unit
    abstract DeliverAsync<'res> : (Callback<'res> -> 'msg) -> Task<'res>

and IAgent<'args, 'model, 'msg, 'req, 'evt> when 'msg :> IMsg and 'req :> IReq and 'evt :> IEvt =
    inherit IAgent<'msg>
    inherit IAgent<'req, 'evt>
    abstract Actor : IActor<'args, 'model, 'req, 'evt> with get
    abstract RunFunc3<'res> : Func<IAgent<'args, 'model, 'msg, 'req, 'evt>, 'res> -> Result<'res, exn>
    abstract AddTask3 : OnFailed<IAgent<'args, 'model, 'msg, 'req, 'evt>> -> GetTask<IAgent<'args, 'model, 'msg, 'req, 'evt>, unit> -> unit
    abstract RunTask3 : OnFailed<IAgent<'args, 'model, 'msg, 'req, 'evt>> -> GetTask<IAgent<'args, 'model, 'msg, 'req, 'evt>, unit> -> unit
    abstract AsAgent2 : IAgent<'req, 'evt> with get
    abstract AsAgent2' : IAgent<'msg> with get

and AgentReq =
    | DoStop of bool * Callback<StopStats>           // forceStop
with interface IReq

and AgentEvt =
    | OnWillStop of Callback<unit>
    | OnDidStop of StopStats
with interface IEvt

type IPack =
    inherit IRunner
    abstract Env : IEnv with get

and IPackAgent<'pack when 'pack :> IPack> =
    inherit IAgent
    abstract Pack : 'pack with get

let DoQuit (forceQuit : bool) callback =
    DoQuit (forceQuit, callback)

let DoAddService (service : IAgent) callback =
    DoAddService (service, callback)

let DoGetService (kind : Kind) (key : Key) callback =
    DoGetService (kind, key, callback)

let TryFindService (kind : Kind) (key : Key) callback =
    TryFindService (kind, key, callback)

let DoRegister (kind : Kind) (spawner : Spawner) callback =
    DoRegister (kind, spawner, callback)

let DoGetAgent (kind : Kind) (key : Key) callback =
    DoGetAgent (kind, key, callback)

let DoStop (forceStop : bool) callback =
    DoStop (forceStop, callback)


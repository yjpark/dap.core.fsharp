[<AutoOpen>]
[<RequireQualifiedAccess>]
module Dap.Platform.Env

open Elmish

open Dap.Prelude
open Dap.Platform.Internal

let private tplSpawnErr = LogEvent.Template3<obj, Ident, Ident>(LogLevelFatal, "[Spawn] {Err}: {ExpectIdent} ~> {ActualIdent}")

let private raiseSpawnErr err expectIdent actualIdent = 
    raiseWith <| tplSpawnErr err expectIdent actualIdent

let private doQuit msg (forceQuit, callback) : EnvOperate =
    fun runner (model, cmd) ->
        logOpError runner "Lifecycle" msg "Not_Implemented" (forceQuit, callback)
        (model, cmd)


let private doAddService msg ((service, callback) : IAgent * Callback<int * int>)
                                : EnvOperate =
    fun runner (model, cmd) ->
        let kind = service.Ident.Kind
        let key = service.Ident.Key
        let replyAck = fun kindServices ->
            let kindServices = kindServices |> Map.add key service
            let services = Map.add kind kindServices model.Services
            reply runner callback <| ack msg ^<| (Map.count services, Map.count kindServices)
            logOpInfo runner "Service" msg "Service_Added" service
            ({model with Services = services}, cmd)
        match Map.tryFind kind model.Services with
        | Some kindServices ->
            match Map.tryFind key kindServices with
            | Some service' ->
                reply runner callback <| nak msg "Already_Exist" service'
                (model, cmd)
            | None ->
                replyAck kindServices
        | None ->
            replyAck Map.empty

let private doGetService msg (kind, key, callback) : EnvOperate =
    fun runner (model, cmd) ->
        match Map.tryFind kind model.Services with
        | Some kindServices ->
            match Map.tryFind key kindServices with
            | Some service ->
                reply runner callback <| ack msg service
            | None ->
                reply runner callback <| nak msg "Key_Not_Exist" kindServices.Count
        | None ->
            reply runner callback <| nak msg "Kind_Not_Exist" model.Services.Count
        (model, cmd)

let private tryFindService msg (kind, key, callback) : EnvOperate =
    fun runner (model, cmd) ->
        match Map.tryFind kind model.Services with
        | Some kindServices ->
            match Map.tryFind key kindServices with
            | Some service ->
                reply runner callback <| ack msg ^<| Some service
            | None ->
                reply runner callback <| ack msg None
        | None ->
            reply runner callback <| ack msg None
        (model, cmd)

let private doRegister msg ((kind, spawner, callback) : Kind * Spawner * Callback<int>)
                                : EnvOperate =
    fun runner (model, cmd) ->
        match Map.tryFind kind model.Spawners with
        | Some spawner ->
            reply runner callback <| nak msg "Already_Registered" spawner
            (model, cmd)
        | None ->
            let spawners = Map.add kind spawner model.Spawners
            reply runner callback <| ack msg ^<| Map.count spawners
            logOpInfo runner "Spawn" msg "Spawner_Registered" spawner
            let agents = Map.add kind Map.empty model.Agents
            ({model with Spawners = spawners; Agents = agents}, cmd)

let private doNewAgent msg ((kind, key, callback) : Kind * Key * Callback<IAgent * bool>)
                            (kindAgents : Map<Key, IAgent>)
                                : EnvOperate =
    fun runner (model, cmd) ->
        match Map.tryFind kind model.Spawners with
        | Some spawner ->
            try
                let ident = identOf runner.Self.Scope kind key
                let agent = spawner ident
                if agent.Ident <> ident
                    then raiseSpawnErr "Invalid_Agent" ident agent.Ident
                logOpInfo runner "Spawn" msg "Agent_Created" agent
                reply runner callback <| ack msg (agent, true)
                let agents = Map.add kind (Map.add key agent kindAgents) model.Agents
                (runner, model, cmd)
                |-|> updateModel ^<| fun m -> {m with Agents = agents}
                |=|> addCmd ^<| EnvEvt ^<| OnNewAgent (kind, key, agent)
            with
            | e ->
                reply runner callback <| nak msg "Spawn_Failed" e
                (model, cmd)
        | None ->
            reply runner callback <| nak msg "Spawner_Not_Exist" kind
            (model, cmd)

let private doGetAgent msg (kind, key, callback) : EnvOperate =
    fun runner (model, cmd) ->
        match Map.tryFind kind model.Agents with
        | Some kindAgents ->
            match Map.tryFind key kindAgents with
            | Some agent ->
                reply runner callback <| ack msg (agent, false)
                (model, cmd)
            | None ->
                (runner, model, cmd)
                |=|> doNewAgent msg (kind, key, callback) kindAgents
        | None ->
            reply runner callback <| nak msg "Not_Registered" model.Spawners.Count
            (model, cmd)

let private handleReq msg (req : EnvReq) : EnvOperate =
    match req with
    | DoQuit (a, b) -> doQuit msg (a, b)
    | DoAddService (a, b) -> doAddService msg (a, b)
    | DoGetService (a, b, c) -> doGetService msg (a, b, c)
    | TryFindService (a, b, c) -> tryFindService msg (a, b, c)
    | DoRegister (a, b, c) -> doRegister msg (a, b, c)
    | DoGetAgent (a, b, c) -> doGetAgent msg (a, b, c)

let private update : Update<IEnv, EnvModel, EnvMsg> =
    fun runner model msg ->
        match msg with
        | EnvReq req -> handleReq msg req
        | EnvEvt _evt -> noOperation
        <| runner <| (model, [])

let private init : Init<IEnv, NoArgs, EnvModel, EnvMsg> =
    fun _runner (_args : NoArgs) ->
        let model = {
            Services = Map.empty
            Spawners = Map.empty
            Agents = Map.empty
        }
        (model, Cmd.none)

let create (param : EnvParam) : IEnv =
    let logic = {
        Init = init
        Update = update
        Subscribe = noSubscription
    }
    let logger = param.Logging.GetLogger param.Scope 
    let env = {
        Platform = param.Platform
        Logging = param.Logging
        Scope = param.Scope
        Clock = param.Clock
        Logger = logger
        Logic = logic
        Stats = statsOfCap <| defaultArg param.GetSlowCap getDefaultSlowCap
        State = None
        Dispatch = None
    }
    param.Platform.Start env
    env :> IEnv

let param platform logging scope clock getSlowCap =
    {
        Platform = platform
        Logging = logging
        Scope = scope
        Clock = clock :> IClock
        GetSlowCap = getSlowCap
    }

let live platform logging scope =
    param platform logging scope (RealClock()) None
    |> create

let play platform logging scope =
    param platform logging scope (FakeClock()) None
    |> create

let register (kind : Kind)
                (spec : AgentSpec<'args, 'model, 'msg, 'req, 'evt>)
                (env : IEnv) =
    let spawner = Agent.getSpawner env spec
    env.Handle <| DoRegister (kind, spawner, None)
    env

let addService kind key spec (env : IEnv) =
    let param = Agent.param env kind key
    let service = Agent.spawn spec param
    env.Handle <| DoAddService (service, None)
    env

let getService (kind : Kind) (key : Key) (env : IEnv) =
    let state = Option.get env.State
    let kindServices = Map.find kind state.Services
    Map.find key kindServices

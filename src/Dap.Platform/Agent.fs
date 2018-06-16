[<AutoOpen>]
[<RequireQualifiedAccess>]
module Dap.Platform.Agent

open Elmish

open Dap.Prelude
open Dap.Platform.Internal

let private tplSpawnErr = LogEvent.Template3<obj, Scope, Ident>(LogLevelFatal, "[Spawn] {Err}: {Scope}  ~> {Ident}")

let private raiseSpawnErr err scope ident = 
    raiseWith <| tplSpawnErr err scope ident

let private doStop msg (forceStop, callback) : AgentOperate<'args, 'model, 'msg, 'req, 'evt> =
    fun runner (model, cmd) ->
        logOpError runner "Lifecycle" msg "Not_Implemented" (forceStop, callback)
        (model, cmd)

let private handleReq msg req : AgentOperate<'args, 'model, 'msg, 'req, 'evt> =
    fun runner (model, cmd) ->
        match req with
        | DoStop (a, b) -> doStop msg (a, b)
        <| runner <| (model, cmd)

let private update wrapper
                    : Update<IAgent, AgentModel<'args, 'model, 'msg, 'req, 'evt>, AgentMsg<'args, 'model, 'msg, 'req, 'evt>> =
    fun runner model msg ->
        match msg with
        | AgentReq req -> handleReq msg req
        | AgentEvt _evt -> noOperation
        | ActorMsg actorMsg -> addSubCmd wrapper actorMsg
        | ActorMsg' op -> op
        <| runner <| (model, [])

let private init wrapper (spec : AgentSpec<'args, 'model, 'msg, 'req, 'evt>)
                            : Init<IAgent, NoArgs, AgentModel<'args, 'model, 'msg, 'req, 'evt>, AgentMsg<'args, 'model, 'msg, 'req, 'evt>> =
    fun runner (args : NoArgs) ->
        let args = spec.Actor.NewArgs ()
        let (actorModel, actorCmd) = spec.Actor.Logic.Init runner args
        let model = {
            Spec = spec
            Actor = actorModel
        }
        (model, Cmd.map wrapper actorCmd)

let private subscribe wrapper (spec : AgentSpec<'args, 'model, 'msg, 'req, 'evt>)
                            : Subscribe<IAgent, AgentModel<'args, 'model, 'msg, 'req, 'evt>, AgentMsg<'args, 'model, 'msg, 'req, 'evt>> =
    fun runner (model : AgentModel<'args, 'model, 'msg, 'req, 'evt>) ->
        Cmd.map wrapper <| spec.Actor.Logic.Subscribe runner model.Actor

let spawn (spec : AgentSpec<'args, 'model, 'msg, 'req, 'evt>)
            (param : AgentParam) : IAgent<'model, 'req, 'evt> =
    let wrapper = wrap ActorMsg' {
        GetSub = fun m -> m.Actor
        SetSub = fun s m -> {m with Actor = s}
        UpdateSub = spec.Actor.Logic.Update
        ReactSub = noReaction
    }
    let logic = {
        Init = init wrapper spec
        Update = update wrapper
        Subscribe = subscribe wrapper spec
    }
    let logger = param.Env.Logging.GetLogger <| String.concat "." [ param.Env.Scope; param.Kind; param.Key ]
    let agent = {
        Spec = spec
        Env = param.Env
        Ident = identOf param.Env.Scope param.Kind param.Key
        Logger = logger
        Logic = logic
        Stats = statsOfCap <| defaultArg spec.GetSlowCap getDefaultSlowCap
        State = None
        Actor = None
        Dispatch = None
    }
    param.Env.Platform.Start agent
    agent :> IAgent<'model, 'req, 'evt>

let param env kind key =
    {
        Env = env
        Kind = kind
        Key = key
    }

let getSpawner (env : IEnv)
                (spec : AgentSpec<'args, 'model, 'msg, 'req, 'evt>) : Spawner =
    fun ident ->
        if ident.Scope = env.Scope then
            param env ident.Kind ident.Key
            |> spawn spec
            :> IAgent
        else
            raiseSpawnErr "Not_In_Same_Scope" env.Scope ident
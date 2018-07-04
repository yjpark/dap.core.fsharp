[<AutoOpen>]
[<RequireQualifiedAccess>]
module Dap.Platform.Agent

open Elmish

open Dap.Prelude
open Dap.Platform.Internal

let private tplSpawnErr = LogEvent.Template3<obj, Scope, Ident>(LogLevelFatal, "[Spawn] {Err}: {Scope}  ~> {Ident}")

type AgentOperate<'args, 'model, 'msg, 'req, 'evt> when 'msg :> IMsg and 'req :> IReq and 'evt :> IEvt =
    Operate<IAgent<'args, 'model, 'msg, 'req, 'evt>, AgentModel<'args, 'model, 'msg, 'req, 'evt>, AgentMsg<'args, 'model, 'msg, 'req, 'evt>>
type AgentInit<'args, 'model, 'msg, 'req, 'evt> when 'msg :> IMsg and 'req :> IReq and 'evt :> IEvt =
    Init<IAgent<'args, 'model, 'msg, 'req, 'evt>, NoArgs, AgentModel<'args, 'model, 'msg, 'req, 'evt>, AgentMsg<'args, 'model, 'msg, 'req, 'evt>>
type AgentUpdate<'args, 'model, 'msg, 'req, 'evt> when 'msg :> IMsg and 'req :> IReq and 'evt :> IEvt =
    Update<IAgent<'args, 'model, 'msg, 'req, 'evt>, AgentModel<'args, 'model, 'msg, 'req, 'evt>, AgentMsg<'args, 'model, 'msg, 'req, 'evt>>
type AgentSubscribe<'args, 'model, 'msg, 'req, 'evt> when 'msg :> IMsg and 'req :> IReq and 'evt :> IEvt =
    Subscribe<IAgent<'args, 'model, 'msg, 'req, 'evt>, AgentModel<'args, 'model, 'msg, 'req, 'evt>, AgentMsg<'args, 'model, 'msg, 'req, 'evt>>

let private raiseSpawnErr err scope ident =
    raiseWith <| tplSpawnErr err scope ident

let private doStop req (forceStop, callback) : AgentOperate<'args, 'model, 'msg, 'req, 'evt> =
    fun runner (model, cmd) ->
        logReqError runner "Lifecycle" req "Not_Implemented" (forceStop, callback)
        (model, cmd)

let private handleReq req : AgentOperate<'args, 'model, 'msg, 'req, 'evt> =
    match req with
    | DoStop (a, b) -> doStop req (a, b)

let private update wrapper
                    : AgentUpdate<'args, 'model, 'msg, 'req, 'evt> =
    fun runner model msg ->
        match msg with
        | AgentReq req ->
            handleReq req
        | AgentEvt _evt ->
            noOperation
        | ActorMsg actorMsg ->
            addSubCmd wrapper actorMsg
        | ActorMsg' wrapping ->
            wrapping.Operate'
        <| runner <| (model, [])

let private init wrapper (spec : AgentSpec<'args, 'model, 'msg, 'req, 'evt>) actorCmd
                            : AgentInit<'args, 'model, 'msg, 'req, 'evt> =
    fun runner (_args : NoArgs) ->
        let model = {
            Spec = spec
        }
        (model, Cmd.map wrapper actorCmd)

let private subscribe wrapper (spec : AgentSpec<'args, 'model, 'msg, 'req, 'evt>) (actorModel : 'model)
                            : AgentSubscribe<'args, 'model, 'msg, 'req, 'evt> =
    fun runner (model : AgentModel<'args, 'model, 'msg, 'req, 'evt>) ->
        Cmd.map wrapper <| spec.Actor.Logic.Subscribe runner actorModel

(*
let private react : AgentReact<'args, 'model, 'msg, 'req, 'evt> =
    fun runner subMsg _subModel model ->
        match runner.Spec.Actor.CastEvt subMsg with
        | Some evt ->
            runner.FireEvent'' evt
        | None ->
            ()
        (model, noCmd)
*)

let spawn (spec : AgentSpec<'args, 'model, 'msg, 'req, 'evt>)
            (param : AgentParam) : IAgent<'args, 'model, 'msg, 'req, 'evt> =
    let ident = Ident.Create param.Env.Scope param.Kind param.Key
    let logger = param.Env.Logging.GetLogger ident.Ident
    let agent = new Agent<'args, 'model, 'msg, 'req, 'evt> (spec, param.Env, ident, logger)
    let (wrapper, actorModel, actorCmd) = agent.SetupActor ()
    let logic : AgentLogic<'args, 'model, 'msg, 'req, 'evt> = {
        Init = init wrapper spec actorCmd
        Update = update wrapper
        Subscribe = subscribe wrapper spec actorModel
    }
    agent.SetLogic logic
    param.Env.Platform.Start agent
    agent.AsAgent

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

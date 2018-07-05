[<AutoOpen>]
[<RequireQualifiedAccess>]
module Dap.Platform.Agent

open Elmish

open Dap.Prelude
open Dap.Platform.Internal.Env
open Dap.Platform.Internal.Agent

let private tplSpawnErr = LogEvent.Template3<obj, Scope, Ident>(LogLevelFatal, "[Spawn] {Err}: {Scope}  ~> {Ident}")

type internal AgentOperate<'args, 'model, 'msg, 'req, 'evt> when 'model : not struct and 'msg :> IMsg and 'req :> IReq and 'evt :> IEvt =
    Operate<IAgent<'args, 'model, 'msg, 'req, 'evt>, AgentModel<'args, 'model, 'msg, 'req, 'evt>, AgentMsg<'args, 'model, 'msg, 'req, 'evt>>
type internal AgentInit<'args, 'model, 'msg, 'req, 'evt> when 'model : not struct and 'msg :> IMsg and 'req :> IReq and 'evt :> IEvt =
    Init<IAgent<'args, 'model, 'msg, 'req, 'evt>, NoArgs, AgentModel<'args, 'model, 'msg, 'req, 'evt>, AgentMsg<'args, 'model, 'msg, 'req, 'evt>>
type internal AgentUpdate<'args, 'model, 'msg, 'req, 'evt> when 'model : not struct and 'msg :> IMsg and 'req :> IReq and 'evt :> IEvt =
    Update<IAgent<'args, 'model, 'msg, 'req, 'evt>, AgentModel<'args, 'model, 'msg, 'req, 'evt>, AgentMsg<'args, 'model, 'msg, 'req, 'evt>>
type internal AgentSubscribe<'args, 'model, 'msg, 'req, 'evt> when 'model : not struct and 'msg :> IMsg and 'req :> IReq and 'evt :> IEvt =
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

let private update : AgentUpdate<'args, 'model, 'msg, 'req, 'evt> =
    fun runner model msg ->
        match msg with
        | AgentReq req ->
            handleReq req
        | AgentEvt _evt ->
            noOperation
        | ActorMsg actorMsg ->
            addSubCmd model.Wrapper actorMsg
        | ActorMsg' wrapping ->
            wrapping.Operate
        <| runner <| (model, [])

let private init (spec : ActorSpec<'args, 'model, 'msg, 'req, 'evt>)
                            : AgentInit<'args, 'model, 'msg, 'req, 'evt> =
    fun runner (_args : NoArgs) ->
        let (actor, cmd, wrapper) = runner |> create' spec ActorMsg' false
        let model = {
            Spec = spec
            Actor = actor
            Wrapper = wrapper
        }
        (model, Cmd.map wrapper cmd)

let private subscribe : AgentSubscribe<'args, 'model, 'msg, 'req, 'evt> =
    fun runner model ->
        Cmd.map model.Wrapper <| model.Spec.Logic.Subscribe runner model.Actor.State

let spawn (spec : ActorSpec<'args, 'model, 'msg, 'req, 'evt>)
            (param : AgentParam) : IAgent<'args, 'model, 'msg, 'req, 'evt> =
    let ident = Ident.Create param.Env.Scope param.Kind param.Key
    let logger = param.Env.Logging.GetLogger ident.Ident
    let logic : AgentLogic<'args, 'model, 'msg, 'req, 'evt> = {
        Init = init spec
        Update = update
        Subscribe = subscribe
    }
    let agent = new Agent<'args, 'model, 'msg, 'req, 'evt> (spec, param.Env, ident, logger, logic)
    param.Env.Platform.Start agent
    agent.AsAgent

let param env kind key =
    {
        Env = env
        Kind = kind
        Key = key
    }

let getSpawner (env : IEnv)
                (spec : ActorSpec<'args, 'model, 'msg, 'req, 'evt>) : Spawner =
    fun ident ->
        if ident.Scope = env.Scope then
            param env ident.Kind ident.Key
            |> spawn spec
            :> IAgent
        else
            raiseSpawnErr "Not_In_Same_Scope" env.Scope ident

let cast<'agent when 'agent :> IAgent> (owner : IOwner) =
    match owner with
    | :? IPart as part ->
        part.Agent
    | :? IAgent as agent ->
        agent
    | _ ->
        raiseWithError "Agent" "Invalid_Owner" (owner.GetType().FullName, typeof<'agent>.FullName)
    |> fun (agent : IAgent) ->
        match agent with
        | :? 'agent as agent ->
            agent
        | _ ->
            raiseWithError "Agent" "Cast_Failed" (agent.GetType().FullName, typeof<'agent>.FullName)

let tryCast<'agent when 'agent :> IAgent> (owner : IOwner) =
    match owner with
    | :? IPart as part ->
        Ok (true, part.Agent)
    | :? IAgent as agent ->
        Ok (false, agent)
    | _ ->
        Error (false, owner.GetType())
    |> Result.bind (fun ((isPart, agent) : bool * IAgent) ->
        match agent with
        | :? 'agent as agent ->
            Ok agent
        | _ ->
            Error (isPart, agent.GetType())
    )
[<RequireQualifiedAccess>]
module Dap.Platform.Agent

open Dap.Prelude
open Dap.Context
open Dap.Platform.Internal.Env
open Dap.Platform.Internal.Agent

let internal spawn<'runner, 'args, 'model, 'msg, 'req, 'evt
                    when 'runner :> BaseAgent<'runner, 'args, 'model, 'msg, 'req, 'evt>
                        and 'model : not struct and 'msg :> IMsg
                        and 'req :> IReq and 'evt :> IEvt>
                    (spec : ActorSpec<'runner, 'args, 'model, 'msg, 'req, 'evt>)
                    (param : AgentParam)
                        : 'runner =
    let logic : AgentLogic<'runner, 'args, 'model, 'msg, 'req, 'evt> = {
        Init = init spec
        Update = update
        Subscribe = subscribe
    }
    let agent = spec.Spawner param
    agent.Setup' spec logic
    agent |> param.Env.Platform.Start
    agent

let internal getSpawner (spec : ActorSpec<'runner, 'args, 'model, 'msg, 'req, 'evt>) : Spawner =
    fun param ->
        spawn spec param :> IAgent

let cast<'agent when 'agent :> IAgent> (owner : IOwner) =
    match owner with
    | :? IPart as part ->
        part.Agent
    | :? IAgent as agent ->
        agent
    | _ ->
        failWith "Invalid_Owner" (owner.GetType().FullName, typeof<'agent>.FullName)
    |> fun (agent : IAgent) ->
        match agent with
        | :? 'agent as agent ->
            agent
        | _ ->
            failWith "Cast_Failed" (agent.GetType().FullName, typeof<'agent>.FullName)

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
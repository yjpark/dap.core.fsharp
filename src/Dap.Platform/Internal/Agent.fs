module Dap.Platform.Internal.Agent

open Elmish

open Dap.Prelude
open Dap.Platform
open Dap.Platform.Internal.Env

type internal AgentOperate<'runner, 'args, 'model, 'msg, 'req, 'evt>
            when 'runner :> IAgent<'args, 'model, 'msg, 'req, 'evt>
                    and 'model : not struct and 'msg :> IMsg
                    and 'req :> IReq and 'evt :> IEvt =
    Operate<'runner, AgentModel<'runner, 'args, 'model, 'msg, 'req, 'evt>, AgentMsg<'runner, 'args, 'model, 'msg, 'req, 'evt>>

type internal AgentInit<'runner, 'args, 'model, 'msg, 'req, 'evt>
            when 'runner :> IAgent<'args, 'model, 'msg, 'req, 'evt>
                    and 'model : not struct and 'msg :> IMsg
                    and 'req :> IReq and 'evt :> IEvt =
    Init<'runner, NoArgs, AgentModel<'runner, 'args, 'model, 'msg, 'req, 'evt>, AgentMsg<'runner, 'args, 'model, 'msg, 'req, 'evt>>

type internal AgentUpdate<'runner, 'args, 'model, 'msg, 'req, 'evt>
            when 'runner :> IAgent<'args, 'model, 'msg, 'req, 'evt>
                    and 'model : not struct and 'msg :> IMsg
                    and 'req :> IReq and 'evt :> IEvt =
    Update<'runner, AgentModel<'runner, 'args, 'model, 'msg, 'req, 'evt>, AgentMsg<'runner, 'args, 'model, 'msg, 'req, 'evt>>

type internal AgentSubscribe<'runner, 'args, 'model, 'msg, 'req, 'evt>
            when 'runner :> IAgent<'args, 'model, 'msg, 'req, 'evt>
                    and 'model : not struct and 'msg :> IMsg
                    and 'req :> IReq and 'evt :> IEvt =
    Subscribe<'runner, AgentModel<'runner, 'args, 'model, 'msg, 'req, 'evt>, AgentMsg<'runner, 'args, 'model, 'msg, 'req, 'evt>>

let private doStop<'runner, 'args, 'model, 'msg, 'req, 'evt
            when 'runner :> IAgent<'args, 'model, 'msg, 'req, 'evt>
                    and 'model : not struct and 'msg :> IMsg
                    and 'req :> IReq and 'evt :> IEvt>
                req (forceStop, callback) : AgentOperate<'runner, 'args, 'model, 'msg, 'req, 'evt> =
    fun runner (model, cmd) ->
        logReqError runner "Lifecycle" req "Not_Implemented" (forceStop, callback)
        (model, cmd)

let private handleReq<'runner, 'args, 'model, 'msg, 'req, 'evt
            when 'runner :> IAgent<'args, 'model, 'msg, 'req, 'evt>
                    and 'model : not struct and 'msg :> IMsg
                    and 'req :> IReq and 'evt :> IEvt>
                req : AgentOperate<'runner, 'args, 'model, 'msg, 'req, 'evt> =
    match req with
    | DoStop (a, b) -> doStop req (a, b)

let internal update<'runner, 'args, 'model, 'msg, 'req, 'evt
            when 'runner :> IAgent<'args, 'model, 'msg, 'req, 'evt>
                    and 'model : not struct and 'msg :> IMsg
                    and 'req :> IReq and 'evt :> IEvt>
                : AgentUpdate<'runner, 'args, 'model, 'msg, 'req, 'evt> =
    fun runner msg model ->
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

let internal init<'runner, 'args, 'model, 'msg, 'req, 'evt
            when 'runner :> IAgent<'args, 'model, 'msg, 'req, 'evt>
                    and 'model : not struct and 'msg :> IMsg
                    and 'req :> IReq and 'evt :> IEvt>
                (spec : ActorSpec<'runner, 'args, 'model, 'msg, 'req, 'evt>)
                            : AgentInit<'runner, 'args, 'model, 'msg, 'req, 'evt> =
    fun runner (_args : NoArgs) ->
        let (actor, wrapper, cmd) = runner |> create' spec ActorMsg' false
        let model = {
            Spec = spec
            Actor = actor
            Wrapper = wrapper
        }
        (model, Cmd.map wrapper cmd)

let internal subscribe<'runner, 'args, 'model, 'msg, 'req, 'evt
            when 'runner :> IAgent<'args, 'model, 'msg, 'req, 'evt>
                    and 'model : not struct and 'msg :> IMsg
                    and 'req :> IReq and 'evt :> IEvt>
                : AgentSubscribe<'runner, 'args, 'model, 'msg, 'req, 'evt> =
    fun runner model ->
        Cmd.map model.Wrapper <| model.Spec.Logic.Subscribe runner model.Actor.State

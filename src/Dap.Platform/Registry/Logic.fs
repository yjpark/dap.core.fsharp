[<AutoOpen>]
[<RequireQualifiedAccess>]
module Dap.Platform.Registry.Logic

open Dap.Prelude
open Dap.Platform

type ActorOperate<'k, 'v when 'k : comparison> = ActorOperate<Args<'k, 'v>, Model<'k, 'v>, Msg<'k, 'v>, Req<'k, 'v>, Evt<'k, 'v>>

let private doGetEntry msg ((key, callback) : 'k * Callback<'v>) : ActorOperate<'k, 'v> =
    fun runner (model, cmd) ->
        match Map.tryFind key model.Entries with
        | Some v ->
            reply runner callback <| ack msg v
        | None ->
            reply runner callback <| nak msg "Not_Exist" ()
        (model, cmd)

let private doSetEntry msg ((key, v, callback) : 'k * 'v * Callback<bool>) : ActorOperate<'k, 'v> =
    fun runner (model, cmd) ->
        let isNew, evt =
            match Map.tryFind key model.Entries with
            | Some v' ->
                false, OnEntryUpdated (key, v', v)
            | None ->
                true, OnEntryAdded (key, v)
        let entries = model.Entries |> Map.add key v
        reply runner callback <| ack msg isNew
        (runner, model, cmd)
        |-|> setModel {model with Entries = entries}
        |=|> addCmd ^<| RegistryEvt evt

let private doAddEntry msg ((key, v, callback) : 'k * 'v * Callback<unit>) : ActorOperate<'k, 'v> =
    fun runner (model, cmd) ->
        match Map.tryFind key model.Entries with
        | Some v' ->
            reply runner callback <| nak msg "Already_Exist" v'
            (model, cmd)
        | None ->
            reply runner callback <| ack msg ()
            let entries = model.Entries |> Map.add key v
            (runner, model, cmd)
            |-|> setModel {model with Entries = entries}
            |=|> addCmd ^<| RegistryEvt ^<| OnEntryAdded (key, v)

let private doRemoveEntry msg ((key, callback) : 'k * Callback<'v>) : ActorOperate<'k, 'v> =
    fun runner (model, cmd) ->
        match Map.tryFind key model.Entries with
        | Some v ->
            reply runner callback <| ack msg v
            let entries = model.Entries |> Map.remove key
            (runner, model, cmd)
            |-|> setModel {model with Entries = entries}
            |=|> addCmd ^<| RegistryEvt ^<| OnEntryRemoved (key, v)
        | None ->
            reply runner callback <| nak msg "Not_Exist" ()
            (model, cmd)

let private tryFindEntry msg ((key, callback) : 'k * Callback<'v option>) : ActorOperate<'k, 'v> =
    fun runner (model, cmd) ->
        let v = Map.tryFind key model.Entries
        reply runner callback <| ack msg v
        (model, cmd)

let private tryRemoveEntry msg ((key, callback) : 'k * Callback<'v option>) : ActorOperate<'k, 'v> =
    fun runner (model, cmd) ->
        match Map.tryFind key model.Entries with
        | Some v ->
            reply runner callback <| ack msg ^<| Some v
            let entries = model.Entries |> Map.remove key
            (runner, model, cmd)
            |-|> setModel {model with Entries = entries}
            |=|> addCmd ^<| RegistryEvt ^<| OnEntryRemoved (key, v)
        | None ->
            reply runner callback <| ack msg None
            (model, cmd)

let private handleReq msg req : ActorOperate<'k, 'v> =
    fun runner (model, cmd) ->
        match req with
        | DoGetEntry (a, b) -> doGetEntry msg (a, b)
        | DoSetEntry (a, b, c) -> doSetEntry msg (a, b, c)
        | DoAddEntry (a, b, c) -> doAddEntry msg (a, b, c)
        | DoRemoveEntry (a, b) -> doRemoveEntry msg (a, b)
        | TryFindEntry (a, b) -> tryFindEntry msg (a, b)
        | TryRemoveEntry (a, b) -> tryRemoveEntry msg (a, b)
        <| runner <| (model, cmd)

let private update : ActorUpdate<Args<'k, 'v>, Model<'k, 'v>, Msg<'k, 'v>, Req<'k, 'v>, Evt<'k, 'v>> =
    fun runner model msg ->
        match msg with
        | RegistryEvt evt ->
            runner.Actor.Args.Event'.Trigger evt
            (model, noCmd)
        | RegistryReq req ->
            (runner, model, [])
            |=|> handleReq msg req

let private init : ActorInit<Args<'k, 'v>, Model<'k, 'v>, Msg<'k, 'v>, Req<'k, 'v>, Evt<'k, 'v>> =
    fun runner _args ->
        ({
            Entries = Map.empty
        }, noCmd)

let logic =
    {
        Init = init
        Update = update
        Subscribe = noSubscription
    }

let getSpec<'k, 'v when 'k : comparison> : AgentSpec<Args<'k, 'v>, Model<'k, 'v>, Msg<'k, 'v>, Req<'k, 'v>, Evt<'k, 'v>> =
    let newArgs = fun owner ->
        {
            Event' = new Bus<Evt<'k, 'v>> (owner)
        }
    {
        Actor =
            {
                NewArgs = newArgs
                Logic = logic
                WrapReq = RegistryReq
                GetOnEvent = fun args -> args.Event'.Publish
            }
        OnAgentEvent = None
        GetSlowCap = None
    }



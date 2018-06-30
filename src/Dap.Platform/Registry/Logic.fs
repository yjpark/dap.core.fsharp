[<AutoOpen>]
[<RequireQualifiedAccess>]
module Dap.Platform.Registry.Logic

open Dap.Prelude
open Dap.Platform

type ActorOperate<'k, 'v when 'k : comparison> = ActorOperate<Args<'k, 'v>, Model<'k, 'v>, Msg<'k, 'v>, Req<'k, 'v>, Evt<'k, 'v>>

let private doGetEntry req ((key, callback) : 'k * Callback<'v>) : ActorOperate<'k, 'v> =
    fun runner (model, cmd) ->
        match Map.tryFind key model.Entries with
        | Some v ->
            reply runner callback <| ack req v
        | None ->
            reply runner callback <| nak req "Not_Exist" ()
        (model, cmd)

let private doSetEntry req ((key, v, callback) : 'k * 'v * Callback<bool>) : ActorOperate<'k, 'v> =
    fun runner (model, cmd) ->
        let isNew, evt =
            match Map.tryFind key model.Entries with
            | Some v' ->
                false, OnEntryUpdated (key, v', v)
            | None ->
                true, OnEntryAdded (key, v)
        let entries = model.Entries |> Map.add key v
        reply runner callback <| ack req isNew
        (runner, model, cmd)
        |-|> setModel {model with Entries = entries}
        |=|> addCmd ^<| RegistryEvt evt

let private doAddEntry req ((key, v, callback) : 'k * 'v * Callback<unit>) : ActorOperate<'k, 'v> =
    fun runner (model, cmd) ->
        match Map.tryFind key model.Entries with
        | Some v' ->
            reply runner callback <| nak req "Already_Exist" v'
            (model, cmd)
        | None ->
            reply runner callback <| ack req ()
            let entries = model.Entries |> Map.add key v
            (runner, model, cmd)
            |-|> setModel {model with Entries = entries}
            |=|> addCmd ^<| RegistryEvt ^<| OnEntryAdded (key, v)

let private doRemoveEntry req ((key, callback) : 'k * Callback<'v>) : ActorOperate<'k, 'v> =
    fun runner (model, cmd) ->
        match Map.tryFind key model.Entries with
        | Some v ->
            reply runner callback <| ack req v
            let entries = model.Entries |> Map.remove key
            (runner, model, cmd)
            |-|> setModel {model with Entries = entries}
            |=|> addCmd ^<| RegistryEvt ^<| OnEntryRemoved (key, v)
        | None ->
            reply runner callback <| nak req "Not_Exist" ()
            (model, cmd)

let private tryFindEntry req ((key, callback) : 'k * Callback<'v option>) : ActorOperate<'k, 'v> =
    fun runner (model, cmd) ->
        let v = Map.tryFind key model.Entries
        reply runner callback <| ack req v
        (model, cmd)

let private tryRemoveEntry req ((key, callback) : 'k * Callback<'v option>) : ActorOperate<'k, 'v> =
    fun runner (model, cmd) ->
        match Map.tryFind key model.Entries with
        | Some v ->
            reply runner callback <| ack req ^<| Some v
            let entries = model.Entries |> Map.remove key
            (runner, model, cmd)
            |-|> setModel {model with Entries = entries}
            |=|> addCmd ^<| RegistryEvt ^<| OnEntryRemoved (key, v)
        | None ->
            reply runner callback <| ack req None
            (model, cmd)

let private handleReq req : ActorOperate<'k, 'v> =
    fun runner (model, cmd) ->
        match req with
        | DoGetEntry (a, b) -> doGetEntry req (a, b)
        | DoSetEntry (a, b, c) -> doSetEntry req (a, b, c)
        | DoAddEntry (a, b, c) -> doAddEntry req (a, b, c)
        | DoRemoveEntry (a, b) -> doRemoveEntry req (a, b)
        | TryFindEntry (a, b) -> tryFindEntry req (a, b)
        | TryRemoveEntry (a, b) -> tryRemoveEntry req (a, b)
        <| runner <| (model, cmd)

let private update : ActorUpdate<Args<'k, 'v>, Model<'k, 'v>, Msg<'k, 'v>, Req<'k, 'v>, Evt<'k, 'v>> =
    fun runner model msg ->
        match msg with
        | RegistryEvt evt ->
            runner.Actor.Args.Event'.Trigger evt
            (model, noCmd)
        | RegistryReq req ->
            (runner, model, [])
            |=|> handleReq req

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



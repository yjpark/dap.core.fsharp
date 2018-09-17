[<AutoOpen>]
[<RequireQualifiedAccess>]
module Dap.Platform.Registry.Logic

open Dap.Prelude
open Dap.Platform
open Dap.Platform.Registry.Types

type ActorOperate<'k, 'v when 'k : comparison> = ActorOperate<Agent<'k, 'v>, Args, Model<'k, 'v>, Msg<'k, 'v>, Req<'k, 'v>, Evt<'k, 'v>>

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
        |-|> updateModel (fun m -> {m with Entries = entries})
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
            |-|> updateModel (fun m -> {m with Entries = entries})
            |=|> addCmd ^<| RegistryEvt ^<| OnEntryAdded (key, v)

let private doRemoveEntry req ((key, callback) : 'k * Callback<'v>) : ActorOperate<'k, 'v> =
    fun runner (model, cmd) ->
        match Map.tryFind key model.Entries with
        | Some v ->
            reply runner callback <| ack req v
            let entries = model.Entries |> Map.remove key
            (runner, model, cmd)
            |-|> updateModel (fun m -> {m with Entries = entries})
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
            |-|> updateModel (fun m -> {m with Entries = entries})
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

let private update : ActorUpdate<Agent<'k, 'v>, Args, Model<'k, 'v>, Msg<'k, 'v>, Req<'k, 'v>, Evt<'k, 'v>> =
    fun runner msg model ->
        match msg with
        | RegistryReq req ->
            handleReq req
        | RegistryEvt _evt ->
            noOperation
        <| runner <| (model, [])

let private init : ActorInit<Args, Model<'k, 'v>, Msg<'k, 'v>> =
    fun _runner _args ->
        ({
            Entries = Map.empty
        }, noCmd)

let spec<'k, 'v when 'k : comparison> args =
    new ActorSpec<Agent<'k, 'v>, Args, Model<'k, 'v>, Msg<'k, 'v>, Req<'k, 'v>, Evt<'k, 'v>>
        (Agent<'k, 'v>.Spawn, args, RegistryReq, castEvt<'k, 'v>, init, update)



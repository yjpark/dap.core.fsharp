[<AutoOpen>]
[<RequireQualifiedAccess>]
module Dap.Platform.Registry.Logic

open Dap.Prelude
open Dap.Platform

let private doGetEntry msg ((key, callback) : 'k * Callback<'v>) : Operate<IRunner, Model<'k, 'v>, Msg<'k, 'v>> =
    fun runner (model, cmd) ->
        match Map.tryFind key model.Entries with
        | Some v ->
            reply runner callback <| ack msg v
        | None ->
            reply runner callback <| nak msg "Not_Exist" ()
        (model, cmd)

let private doSetEntry msg ((key, v, callback) : 'k * 'v * Callback<bool>) : Operate<IRunner, Model<'k, 'v>, Msg<'k, 'v>> =
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

let private doAddEntry msg ((key, v, callback) : 'k * 'v * Callback<unit>) : Operate<IRunner, Model<'k, 'v>, Msg<'k, 'v>> =
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

let private doRemoveEntry msg ((key, callback) : 'k * Callback<'v>) : Operate<IRunner, Model<'k, 'v>, Msg<'k, 'v>> =
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

let private tryFindEntry msg ((key, callback) : 'k * Callback<'v option>) : Operate<IRunner, Model<'k, 'v>, Msg<'k, 'v>> =
    fun runner (model, cmd) ->
        let v = Map.tryFind key model.Entries
        reply runner callback <| ack msg v
        (model, cmd)

let private tryRemoveEntry msg ((key, callback) : 'k * Callback<'v option>) : Operate<IRunner, Model<'k, 'v>, Msg<'k, 'v>> =
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

let private handleReq msg req : Operate<IRunner, Model<'k, 'v>, Msg<'k, 'v>> =
    fun runner (model, cmd) ->
        match req with
        | DoGetEntry (a, b) -> doGetEntry msg (a, b)
        | DoSetEntry (a, b, c) -> doSetEntry msg (a, b, c)
        | DoAddEntry (a, b, c) -> doAddEntry msg (a, b, c)
        | DoRemoveEntry (a, b) -> doRemoveEntry msg (a, b)
        | TryFindEntry (a, b) -> tryFindEntry msg (a, b)
        | TryRemoveEntry (a, b) -> tryRemoveEntry msg (a, b)
        <| runner <| (model, cmd)

let private update : Update<IRunner, Model<'k, 'v>, Msg<'k, 'v>> =
    fun runner model msg -> 
        match msg with
        | RegistryEvt evt ->
            model.Event'.Trigger evt
            (model, noCmd)
        | RegistryReq req ->
            (runner, model, [])
            |=|> handleReq msg req

let private init : Init<IAgent, NoArgs, Model<'k, 'v>, Msg<'k, 'v>> =
    fun _runner _args ->
        ({
            Event' = new Event<Evt<'k, 'v>> ()
            Entries = Map.empty
        }, noCmd)

let logic : Logic<IAgent, NoArgs, Model<'k, 'v>, Msg<'k, 'v>> =
    {
        Init = init
        Update = update
        Subscribe = noSubscription
    }

let getSpec<'k, 'v when 'k : comparison> : AgentSpec<NoArgs, Model<'k, 'v>, Msg<'k, 'v>, Req<'k, 'v>, Evt<'k, 'v>> =
    {
        Actor =
            {
                NewArgs = fun _ -> NoArgs
                Logic = logic
                WrapReq = RegistryReq
                GetOnEvent = fun model -> model.Event'.Publish
            }
        OnAgentEvent = None
        GetSlowCap = None
    }



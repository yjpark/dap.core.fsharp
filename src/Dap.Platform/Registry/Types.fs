[<AutoOpen>]
[<RequireQualifiedAccess>]
module Dap.Platform.Registry.Types

open Dap.Platform

type Model<'k, 'v when 'k : comparison> = {
    Event' : Bus<Evt<'k, 'v>>
    Entries : Map<'k, 'v>
}

and Req<'k, 'v> =
    | DoGetEntry of 'k * Callback<'v>
    | DoSetEntry of 'k * 'v * Callback<bool> // -> isNew
    | DoAddEntry of 'k * 'v * Callback<unit>
    | DoRemoveEntry of 'k * Callback<'v>
    | TryFindEntry of 'k * Callback<'v option>
    | TryRemoveEntry of 'k * Callback<'v option>

and Evt<'k, 'v> =
    | OnEntryAdded of 'k * 'v
    | OnEntryRemoved of 'k * 'v
    | OnEntryUpdated of 'k * 'v * 'v

and Msg<'k, 'v> =
    | RegistryReq of Req<'k, 'v>
    | RegistryEvt of Evt<'k, 'v>
with
    interface IMsg

let DoGetEntry' key callback =
    DoGetEntry (key, callback)

let DoSetEntry' key v callback =
    DoSetEntry (key, v, callback)

let DoAddEntry' key v callback =
    DoAddEntry (key, v, callback)

let DoRemoveEntry' key callback =
    DoRemoveEntry (key, callback)

let TryFindEntry' key callback =
    TryFindEntry (key, callback)

let TryRemoveEntry' key callback =
    TryRemoveEntry (key, callback)
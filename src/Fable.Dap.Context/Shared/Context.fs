[<AutoOpen>]
[<RequireQualifiedAccess>]
module Dap.Context.Context

open Dap.Prelude
open Dap.Context.Internal

let create' logging kind propertiesSpawner =
    ContextSpec.Create (kind, propertiesSpawner)
    |> Context.Create logging
    :> IContext

let create kind propertiesSpawner =
    create' (getLogging ()) kind propertiesSpawner

let combo kind =
    fun owner ->
        Properties.combo owner NoKey
        :> IProperties
    |> create kind
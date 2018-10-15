[<RequireQualifiedAccess>]
module Dap.Context.Context

open Dap.Prelude
open Dap.Context

let map<'p when 'p :> IProperty> kind (spawner : PropertySpawner<'p>) =
    new DictContext<'p> (getLogging (), kind, spawner)

let list<'p when 'p :> IProperty> kind (spawner : PropertySpawner<'p>) =
    new ListContext<'p> (getLogging (), kind, spawner)

let combo kind =
    new ComboContext (getLogging (), kind)

let custom<'p when 'p :> ICustomProperties> kind (spawner : PropertySpawner<'p>) =
    new CustomContext<'p> (getLogging(), kind, spawner)

let map0<'p when 'p :> IProperty> kind (spawner : PropertySpawner) =
    fun o k -> spawner o k :?> 'p
    |> map<'p> kind

let list0<'p when 'p :> IProperty> kind (spawner : PropertySpawner) =
    fun o k -> spawner o k :?> 'p
    |> list<'p> kind

let custom0<'p when 'p :> ICustomProperties> kind (spawner : PropertySpawner) =
    fun o k -> spawner o k :?> 'p
    |> custom<'p> kind

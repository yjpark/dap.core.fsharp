[<RequireQualifiedAccess>]
module Dap.Context.Properties

open Dap.Prelude
open Dap.Context
open Dap.Context.Internal

let dict<'p when 'p :> IProperty> (spawner : PropertySpawner<'p>) (owner : IOwner, key : Key) =
    DictProperty<'p>.Create (owner, Property.dictSpec<'p> key E.emptyObject spawner)
    :> IDictProperty<'p>

let list<'p when 'p :> IProperty> (spawner : PropertySpawner<'p>) (owner : IOwner, key : Key) =
    ListProperty<'p>.Create (owner, Property.listSpec<'p> key E.emptyList spawner)
    :> IListProperty<'p>

let combo (owner : IOwner, key : Key) =
    ComboProperty.Create (owner, Property.comboSpec key <| E.object [])
    :> IComboProperty

let custom<'p when 'p :> IProperty> (spawner : PropertySpawner<'p>) (owner : IOwner) (key : Key) =
    spawner (owner, key)

[<RequireQualifiedAccess>]
module Dap.Context.Properties

open Dap.Prelude
open Dap.Context
open Dap.Context.Internal

let dict<'p when 'p :> IProperty> (spawner : PropertySpawner<'p>) (owner : IOwner) (key : Key) =
    Property.dictSpec<'p> key E.emptyObject spawner
    |> DictProperty<'p>.Create owner
    :> IDictProperty<'p>

let list<'p when 'p :> IProperty> (spawner : PropertySpawner<'p>) (owner : IOwner) (key : Key) =
    Property.listSpec<'p> key E.emptyList spawner
    |> ListProperty<'p>.Create owner
    :> IListProperty<'p>

let combo (owner : IOwner) (key : Key) =
    Property.comboSpec key <| E.object []
    |> ComboProperty.Create owner
    :> IComboProperty

let custom<'p when 'p :> IProperty> (spawner : PropertySpawner<'p>) (owner : IOwner) (key : Key) =
    spawner owner key

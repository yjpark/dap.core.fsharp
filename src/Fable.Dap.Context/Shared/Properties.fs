[<AutoOpen>]
[<RequireQualifiedAccess>]
module Dap.Context.Properties

open Dap.Prelude
open Dap.Context.Internal

let map<'p when 'p :> IProperty> (spawner : PropertySpawner<'p>) (owner : IOwner) (key : Key) =
    IPropertySpec<'p>.Create (key, E.object [], spawner)
    |> MapProperty<'p>.Create owner
    :> IMapProperty<'p>

let list<'p when 'p :> IProperty> (spawner : PropertySpawner<'p>) (owner : IOwner) (key : Key) =
    IPropertySpec<'p>.Create (key, E.list [], spawner)
    |> ListProperty<'p>.Create owner
    :> IListProperty<'p>

let combo (owner : IOwner) (key : Key) =
    IPropertySpec.Create (key, E.object [])
    |> ComboProperty.Create owner
    :> IComboProperty

type IComboProperty with
    static member Empty owner = combo owner NoKey
    member this.AddBool key initValue validator =
        Property.boolSpec key initValue None
        |> this.AddVar<bool>
    member this.AddInt key initValue validator =
        Property.intSpec key initValue None
        |> this.AddVar<int>
#if !FABLE_COMPILER
    member this.AddLong key initValue validator =
        Property.longSpec key initValue None
        |> this.AddVar<int64>
#endif
    member this.AddString key initValue validator =
        Property.stringSpec key initValue None
        |> this.AddVar<string>

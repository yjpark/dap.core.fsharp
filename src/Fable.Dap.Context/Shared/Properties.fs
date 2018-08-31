[<AutoOpen>]
[<RequireQualifiedAccess>]
module Dap.Context.Properties

open Dap.Prelude
open Dap.Context.Internal

let map<'p when 'p :> IProperty> (spawner : PropertySpawner<'p>) (owner : IOwner) (key : Key) =
    Property.mapSpec<'p> key (E.object []) spawner
    |> MapProperty<'p>.Create owner
    :> IMapProperty<'p>

let list<'p when 'p :> IProperty> (spawner : PropertySpawner<'p>) (owner : IOwner) (key : Key) =
    Property.listSpec<'p> key (E.list []) spawner
    |> ListProperty<'p>.Create owner
    :> IListProperty<'p>

let combo (owner : IOwner) (key : Key) =
    Property.comboSpec key <| E.object []
    |> ComboProperty.Create owner
    :> IComboProperty

let custom<'p when 'p :> IProperty> (owner : IOwner) (kind : Kind) (key : Key) (spawner : PropertySpawner<'p>) =
    Property.customSpec<'p> kind key (E.object []) spawner
    |> ComboProperty.Create owner
    :> IComboProperty

type IComboProperty with
    static member Empty owner = combo owner NoKey
    member this.AddCombo key =
        Property.comboSpec key <| E.object []
        |> this.AddCombo
    member this.AddBool key initValue validator =
        Property.boolSpec key initValue validator
        |> this.AddVar<bool>
    member this.AddInt key initValue validator =
        Property.intSpec key initValue validator
        |> this.AddVar<int>
#if !FABLE_COMPILER
    member this.AddLong key initValue validator =
        Property.longSpec key initValue validator
        |> this.AddVar<int64>
#endif
    member this.AddString key initValue validator =
        Property.stringSpec key initValue validator
        |> this.AddVar<string>

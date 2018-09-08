[<AutoOpen>]
[<RequireQualifiedAccess>]
module Dap.Context.Properties

open Dap.Prelude
open Dap.Context.Internal

let map<'p when 'p :> IProperty> (spawner : PropertySpawner<'p>) (owner : IOwner) (key : Key) =
    Property.mapSpec<'p> key E.emptyObject spawner
    |> MapProperty<'p>.Create owner
    :> IMapProperty<'p>

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

type IComboProperty with
    static member Empty () = combo noOwner NoKey
    member this.ValueAsList =
        this.Value |> Map.toList |> List.map snd
    member this.AddMap<'p when 'p :> IProperty> (spawner, key) =
        Property.mapSpec<'p> key E.emptyObject spawner
        |> this.AddMap<'p>
    member this.AddList<'p when 'p :> IProperty> (spawner, key) =
        Property.listSpec<'p> key E.emptyList spawner
        |> this.AddList<'p>
    member this.AddCustom<'p when 'p :> ICustomProperty> (spawner, key) =
        Property.customSpec<'p> key E.emptyObject spawner
        |> this.AddCustom<'p>
    member this.AddCombo key =
        Property.comboSpec key <| E.object []
        |> this.AddCombo
    member this.AddComboMap key =
        this.AddMap<IComboProperty> (combo, key)
    member this.AddComboList key =
        this.AddList<IComboProperty> (combo, key)
    member this.AddVar<'v> (encoder, decoder, key, initValue, validator) =
        Property.varSpec<'v> encoder decoder key initValue validator
        |> this.AddVar<'v>
    member this.AddVar<'v> (encoder, decoder, key, initValue) =
        this.AddVar<'v> (encoder, decoder, key, initValue, None)
    member this.AddBool (key, initValue, validator) =
        Property.boolSpec key initValue validator
        |> this.AddVar<bool>
    member this.AddBool (key, initValue) =
        this.AddBool (key, initValue, None)
    member this.AddString (key, initValue, validator) =
        Property.stringSpec key initValue validator
        |> this.AddVar<string>
    member this.AddString (key, initValue) =
        this.AddString (key, initValue, None)
    member this.AddInt (key, initValue, validator) =
        Property.intSpec key initValue validator
        |> this.AddVar<int>
    member this.AddInt (key, initValue) =
        this.AddInt (key, initValue, None)
#if !FABLE_COMPILER
    member this.AddLong (key, initValue, validator) =
        Property.longSpec key initValue validator
        |> this.AddVar<int64>
    member this.AddLong (key, initValue) =
        this.AddLong (key, initValue, None)
#endif
    member this.AddDecimal (key, initValue, validator) =
        Property.decimalSpec key initValue validator
        |> this.AddVar<decimal>
    member this.AddDecimal (key, initValue) =
        this.AddDecimal (key, initValue, None)

type IMapProperty<'p when 'p :> IProperty> with
    static member Empty spawner = map<'p> spawner noOwner NoKey

type IListProperty<'p when 'p :> IProperty> with
    static member Empty spawner = list<'p> spawner noOwner NoKey

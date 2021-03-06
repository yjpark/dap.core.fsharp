[<AutoOpen>]
module Dap.Context.PropertiesExtension

open Dap.Prelude
open Dap.Context
open Dap.Context.Internal

type IComboProperty with
    static member Create (o, k) = Properties.combo (o, k)
    static member Create () = Properties.combo (noOwner, NoKey)

type IComboProperty with
    member this.AddDict<'p when 'p :> IProperty> (spawner, key) =
        Property.dictSpec<'p> key E.emptyObject spawner
        |> this.AddDict<'p>
    member this.AddDict<'v> (encoder, decoder, key, initValue, validator) =
        Property.varSpec<'v> encoder decoder key initValue validator
        |> this.AddDict<IVarProperty<'v>>
    member this.AddDict<'v> (encoder, decoder, key, initValue) =
        this.AddDict<'v> (encoder, decoder, key, initValue, None)

type IComboProperty with
    member this.AddList<'p when 'p :> IProperty> (spawner, key) =
        Property.listSpec<'p> key E.emptyList spawner
        |> this.AddList<'p>
    member this.AddList<'v> (encoder, decoder, key, initValue, validator) =
        Property.varSpec<'v> encoder decoder key initValue validator
        |> this.AddList<IVarProperty<'v>>
    member this.AddList<'v> (encoder, decoder, key, initValue) =
        this.AddList<'v> (encoder, decoder, key, initValue, None)

type IComboProperty with
    member this.AddCustom<'p when 'p :> ICustomProperty> (spawner, key) =
        Property.customSpec<'p> key E.emptyObject spawner
        |> this.AddCustom<'p>

type IComboProperty with
    member this.AddCombo key =
        Property.comboSpec key <| E.object []
        |> this.AddCombo
    member this.AddComboDict key =
        this.AddDict<IComboProperty> (Properties.combo, key)
    member this.AddComboList key =
        this.AddList<IComboProperty> (Properties.combo, key)

type IComboProperty with
    member this.AddVar<'v> (encoder, decoder, key, initValue, validator) =
        Property.varSpec<'v> encoder decoder key initValue validator
        |> this.AddVar<'v>
    member this.AddVar<'v> (encoder, decoder, key, initValue) =
        this.AddVar<'v> (encoder, decoder, key, initValue, None)

type IComboProperty with
    member this.AddBool (key, initValue, validator) =
        Property.boolSpec key initValue validator
        |> this.AddVar<bool>
    member this.AddBool (key, initValue) =
        this.AddBool (key, initValue, None)

type IComboProperty with
    member this.AddString (key, initValue, validator) =
        Property.stringSpec key initValue validator
        |> this.AddVar<string>
    member this.AddString (key, initValue) =
        this.AddString (key, initValue, None)

type IComboProperty with
    member this.AddInt (key, initValue, validator) =
        Property.intSpec key initValue validator
        |> this.AddVar<int>
    member this.AddInt (key, initValue) =
        this.AddInt (key, initValue, None)
#if !FABLE_COMPILER

type IComboProperty with
    member this.AddLong (key, initValue, validator) =
        Property.longSpec key initValue validator
        |> this.AddVar<int64>
    member this.AddLong (key, initValue) =
        this.AddLong (key, initValue, None)
#endif

type IComboProperty with
    member this.AddDecimal (key, initValue, validator) =
        Property.decimalSpec key initValue validator
        |> this.AddVar<decimal>
    member this.AddDecimal (key, initValue) =
        this.AddDecimal (key, initValue, None)

type IDictProperty<'p when 'p :> IProperty> with
    static member Default spawner = Properties.dict<'p> spawner (noOwner, NoKey)

type IListProperty<'p when 'p :> IProperty> with
    static member Default spawner = Properties.list<'p> spawner (noOwner, NoKey)

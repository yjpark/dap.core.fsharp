[<RequireQualifiedAccess>]
module Dap.Context.Builder.Combo

open Microsoft.FSharp.Quotations

open Dap.Prelude
open Dap.Context

type Builder () =
    inherit ObjBuilder<IComboProperty> ()
    override __.Zero () =
        IComboProperty.Default ()
    [<CustomOperation("option")>]
    member __.Option (combo : IComboProperty, prop : IVarProperty) =
        combo
    [<CustomOperation("custom")>]
    member __.Custom (combo : IComboProperty, key, prop : ICustomProperty) =
        combo.AddAny key prop.Clone0 |> ignore
        combo
    [<CustomOperation("combo")>]
    member __.Combo (combo : IComboProperty, key, prop : IComboProperty) =
        combo.AddAny key prop.Clone0 |> ignore
        combo
    member __.Bool (combo : IComboProperty, key, initValue, validator) =
        combo.AddBool (key, initValue, validator) |> ignore
        combo
    [<CustomOperation("bool''")>]
    member this.Bool'' (combo, key, initValue, validator) =
        this.Bool (combo, key, initValue, Some validator)
    [<CustomOperation("bool'")>]
    member this.Bool' (combo, key, initValue) =
        this.Bool (combo, key, initValue, None)
    [<CustomOperation("bool")>]
    member this.Bool (combo, key) =
        this.Bool (combo, key, false, None)
    member __.String (combo: IComboProperty, key, initValue, validator) =
        combo.AddString (key, initValue, validator) |> ignore
        combo
    [<CustomOperation("string''")>]
    member this.String'' (combo, key, initValue, validator) =
        this.String (combo, key, initValue, Some validator)
    [<CustomOperation("string'")>]
    member this.String' (combo, key, initValue) =
        this.String (combo, key, initValue, None)
    [<CustomOperation("string")>]
    member this.String (combo, key) =
        this.String (combo, key, "", None)
    member __.Int (combo : IComboProperty, key, initValue, validator) =
        combo.AddInt (key, initValue, validator) |> ignore
        combo
    [<CustomOperation("int''")>]
    member this.Int'' (combo, key, initValue, validator) =
        this.Int (combo, key, initValue, Some validator)
    [<CustomOperation("int'")>]
    member this.Int' (combo, key, initValue) =
        this.Int (combo, key, initValue, None)
    [<CustomOperation("int")>]
    member this.Int (combo, key) =
        this.Int (combo, key, 0, None)
#if !FABLE_COMPILER
    member __.Long (combo : IComboProperty, key, initValue, validator) =
        combo.AddLong (key, initValue, validator) |> ignore
        combo
    [<CustomOperation("long''")>]
    member this.Long'' (combo, key, initValue, validator) =
        this.Long (combo, key, initValue, Some validator)
    [<CustomOperation("long'")>]
    member this.Long' (combo, key, initValue) =
        this.Long (combo, key, initValue, None)
    [<CustomOperation("long")>]
    member this.Long (combo, key) =
        this.Long (combo, key, 0L, None)
#endif
    member __.Decimal (combo: IComboProperty, key, initValue, validator) =
        combo.AddDecimal (key, initValue, validator) |> ignore
        combo
    [<CustomOperation("decimal''")>]
    member this.Decimal'' (combo, key, initValue, validator) =
        this.Decimal (combo, key, initValue, Some validator)
    [<CustomOperation("decimal'")>]
    member this.Decimal' (combo, key, initValue) =
        this.Decimal (combo, key, initValue, None)
    [<CustomOperation("decimal")>]
    member this.Decimal (combo, key) =
        this.Decimal (combo, key, 0M, None)

type ExtendBuilder (parent : IComboProperty) =
    inherit Builder ()
    override __.Zero () = parent.Clone noOwner NoKey
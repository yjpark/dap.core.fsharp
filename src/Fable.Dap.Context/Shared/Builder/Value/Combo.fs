[<RequireQualifiedAccess>]
module Dap.Context.Builder.Value.Combo

open System.Reflection

open Dap.Prelude
open Dap.Context

type Builder () =
    member this.Yield (_ : 'a) = this.Zero ()
    member __.Zero () =
        IComboProperty.Empty noOwner
    [<CustomOperation("custom")>]
    member __.Custom (this : IComboProperty, key, prop : ICustomProperty) =
        this.AddAny key prop.Clone0 |> ignore
        this
    [<CustomOperation("combo")>]
    member __.Combo (this : IComboProperty, key, prop : IComboProperty) =
        this.AddAny key prop.Clone0 |> ignore
        this
    [<CustomOperation("bool")>]
    member __.Bool (this : IComboProperty, key, initValue, validator) =
        this.AddBool key initValue validator |> ignore
        this
    [<CustomOperation("int")>]
    member __.Int (this : IComboProperty, key, initValue, validator) =
        this.AddInt key initValue validator |> ignore
        this
#if !FABLE_COMPILER
    [<CustomOperation("long")>]
    member __.Long (this : IComboProperty, key, initValue, validator) =
        this.AddLong key initValue validator |> ignore
        this
#endif
    [<CustomOperation("string")>]
    member __.String (this: IComboProperty, key, initValue, validator) =
        this.AddString key initValue validator |> ignore
        this
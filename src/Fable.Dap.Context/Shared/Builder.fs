module Dap.Context.Builder

open Dap.Prelude
open Dap.Context

[<AbstractClass>]
type ObjBuilder<'obj when 'obj :> IObj> () =
    member this.Yield (_ : 'a) =
        this.Zero ()
    abstract member Zero : unit -> 'obj

type ComboBuilder () =
    inherit ObjBuilder<IComboProperty> ()
    override __.Zero () =
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

type ContextBuilder (kind') =
    inherit ObjBuilder<IContext> ()
    let kind : Kind = kind'
    override __.Zero () =
        IContext.Empty kind
    [<CustomOperation("properties")>]
    member __.Properties (_: IContext, properties : IComboProperty) =
        Context.combo kind
        |> fun context ->
            properties.SyncTo context.Properties |> ignore
            context

let combo = new ComboBuilder ()

let context kind = new ContextBuilder (kind)

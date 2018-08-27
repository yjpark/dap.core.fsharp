[<AutoOpen>]
module Dap.Context.CustomProperty

open Dap.Prelude
open Dap.Context.Unsafe
open Dap.Context.Internal

[<AbstractClass>]
type CustomProperty<'p when 'p :> ICustomProperty> () =
    let mutable spec : PropertySpec option = None
    let mutable target : IProperty option = None
    member _this.Setup target' =
        if target.IsSome then
            failWith "Already_Setup" (spec, target, target')
        target <- Some target'
        spec <-
            target'.ToJson ()
            |> PropertySpec.Create target'.Spec.Luid target'.Spec.Key
            |> Some
    abstract member Self : 'p with get
    abstract member Spawn : IOwner -> Key -> 'p
    member this.AsCustomProperty = this :> ICustomProperty<'p>
    member this.AsCustomProperty0 = this.Self :> ICustomProperty
    member this.AsProperty = this :> IProperty
    member this.Spec = spec |> Option.get :> IPropertySpec
    member this.Target = target |> Option.get
    interface ICustomProperty<'p> with
        member this.Self = this.Self
        member this.Clone o k =
            let clone = this.Spawn o k
            let clone' = (clone :> IProperty)
            this.AsProperty.ToJson () |> clone'.WithJson |> ignore
            if this.AsProperty.Sealed then clone'.Seal ()
            clone
    interface IProperty with
        member _this.Kind = PropertyKind.CustomProperty
        member this.Ver = this.Target.Ver
        member this.Spec = this.Spec
        member this.Seal () = this.Target.Seal ()
        member this.Sealed = this.Target.Sealed
        member this.WithJson json = this.Target.WithJson json
        member this.OnChanged0 = this.Target.OnChanged0
        member this.Clone0 o k = this.AsCustomProperty.Clone o k :> IProperty
    interface IJson with
        member this.ToJson () = this.Target.ToJson ()
    interface IUnsafeProperty with
        member this.AsVar = (this.Target :?> IUnsafeProperty) .AsVar
        member this.AsMap = (this.Target :?> IUnsafeProperty) .AsMap
        member this.AsList = (this.Target :?> IUnsafeProperty) .AsList
        member this.AsCombo = (this.Target :?> IUnsafeProperty) .AsCombo
    #if FABLE_COMPILER
        [<PassGenericsAttribute>]
    #endif
        member this.ToVar<'v1> () = (this.Target :?> IUnsafeProperty) .ToVar<'v1> ()
    #if FABLE_COMPILER
        [<PassGenericsAttribute>]
    #endif
        member this.ToMap<'v1> () = (this.Target :?> IUnsafeProperty) .ToMap<'v1> ()
    #if FABLE_COMPILER
        [<PassGenericsAttribute>]
    #endif
        member this.ToList<'v1> () = (this.Target :?> IUnsafeProperty) .ToList<'v1> ()


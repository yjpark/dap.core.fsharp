[<AutoOpen>]
module Dap.Context.CustomProperty

#if FABLE_COMPILER
open Fable.Core
#endif

open Dap.Prelude
open Dap.Context.Unsafe
open Dap.Context.Internal

[<AbstractClass>]
type WrapProperty<'p when 'p :> ICustomProperty> () =
    let mutable spec : IPropertySpec option = None
    let mutable target : IProperty option = None
    member __.Setup target' =
        if target.IsSome then
            failWith "Already_Setup" (spec, target, target')
        target <- Some target'
        spec <-
            let initValue = target'.ToJson ()
            new PropertySpec (target'.Spec.Luid, target'.Spec.Key, initValue)
            :> IPropertySpec
            |> Some
    abstract member Self : 'p with get
    abstract member Spawn : IOwner -> Key -> 'p
    member this.AsCustomProperty = this :> ICustomProperty<'p>
    member this.AsCustomProperty0 = this.Self :> ICustomProperty
    member this.AsProperty = this :> IProperty
    member this.Spec = spec |> Option.get
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
        member __.Kind = PropertyKind.CustomProperty
        member this.Ver = this.Target.Ver
        member this.Spec = this.Spec
        member this.Seal () = this.Target.Seal ()
        member this.Sealed = this.Target.Sealed
        member this.WithJson json = this.Target.WithJson json
        member this.OnChanged = this.Target.OnChanged
        member this.Clone0 o k = this.AsCustomProperty.Clone o k :> IProperty
    interface IJson with
        member this.ToJson () = this.Target.ToJson ()
    interface IUnsafeProperty with
        member this.AsVar = (this.Target :?> IUnsafeProperty) .AsVar
        member this.AsMap = (this.Target :?> IUnsafeProperty) .AsMap
        member this.AsList = (this.Target :?> IUnsafeProperty) .AsList
        member this.AsCombo = (this.Target :?> IUnsafeProperty) .AsCombo
        member this.AsCustom = (this.Target :?> IUnsafeProperty) .AsCustom
    #if FABLE_COMPILER
        [<PassGenericsAttribute>]
    #endif
        member this.ToVar<'v1> () = (this.Target :?> IUnsafeProperty) .ToVar<'v1> ()
    #if FABLE_COMPILER
        [<PassGenericsAttribute>]
    #endif
        member this.ToMap<'p1 when 'p1 :> IProperty> () = (this.Target :?> IUnsafeProperty) .ToMap<'p1> ()
    #if FABLE_COMPILER
        [<PassGenericsAttribute>]
    #endif
        member this.ToList<'p1 when 'p1 :> IProperty> () = (this.Target :?> IUnsafeProperty) .ToList<'p1> ()
#if FABLE_COMPILER
        [<PassGenericsAttribute>]
#endif
        member this.ToCustom<'p1 when 'p1 :> ICustomProperty> () = (this.Target :?> IUnsafeProperty) .ToCustom<'p1> ()


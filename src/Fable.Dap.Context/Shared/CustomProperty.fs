[<AutoOpen>]
module Dap.Context.CustomProperty

#if FABLE_COMPILER
open Fable.Core
#endif

open Dap.Prelude
open Dap.Context.Unsafe
open Dap.Context.Internal

[<AbstractClass>]
type WrapProperty<'p, 't when 'p :> ICustomProperty and 't :> IProperty> () =
    let mutable spec : IPropertySpec option = None
    let mutable target : 't option = None
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
    abstract member SyncTo : 'p -> unit
    member this.AsCustomProperty = this :> ICustomProperty<'p>
    member this.AsCustom = this.Self :> ICustomProperty
    member this.AsProperty = this :> IProperty
    member __.Spec = spec |> Option.get
    member __.Target = target |> Option.get
    member __.UnsafeTarget = target |> Option.get :> IProperty :?> IUnsafeProperty
    interface ICustomProperty<'p> with
        member this.Self = this.Self
        member this.SyncTo other = this.SyncTo other
        member this.Clone o k =
            this.Spawn o k
            |> this.SetupClone<'p> (Some this.SyncTo)
            |> fun clone ->
                if this.AsProperty.Sealed then
                    (clone :> IProperty) .Seal ()
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
#if FABLE_COMPILER
        [<PassGenericsAttribute>]
#endif
        member this.SyncTo0 t =
            let this' = this.AsProperty
            if this'.Kind <> t.Kind then
                this'.Owner.Log <| tplPropertyError "Property:InValid_Kind" this'.Spec.Luid this'.Ver (this'.Kind, t.Kind, t)
            elif this.GetType () <> t.GetType () then
                this'.Owner.Log <| tplPropertyError "Property:InValid_Type" this'.Spec.Luid this'.Ver (this.GetType (), t.GetType (), t)
            else
                this.SyncTo (t :?> 'p)
    interface IAspect with
        member this.Owner = this.Target.Owner
    interface IJson with
        member this.ToJson () = this.Target.ToJson ()
    interface IUnsafeProperty with
        member this.AsVar = this.UnsafeTarget.AsVar
        member this.AsMap = this.UnsafeTarget.AsMap
        member this.AsList = this.UnsafeTarget.AsList
        member this.AsCombo = this.UnsafeTarget.AsCombo
        member this.AsCustom = this.UnsafeTarget.AsCustom
    #if FABLE_COMPILER
        [<PassGenericsAttribute>]
    #endif
        member this.ToVar<'v1> () = this.UnsafeTarget.ToVar<'v1> ()
    #if FABLE_COMPILER
        [<PassGenericsAttribute>]
    #endif
        member this.ToDict<'p1 when 'p1 :> IProperty> () = this.UnsafeTarget.ToDict<'p1> ()
    #if FABLE_COMPILER
        [<PassGenericsAttribute>]
    #endif
        member this.ToList<'p1 when 'p1 :> IProperty> () = this.UnsafeTarget.ToList<'p1> ()
#if FABLE_COMPILER
        [<PassGenericsAttribute>]
#endif
        member this.ToCustom<'p1 when 'p1 :> ICustomProperty> () = this.UnsafeTarget.ToCustom<'p1> ()

[<AbstractClass>]
type WrapProperties<'p, 't when 'p :> ICustomProperty and 't :> IProperties> () =
    inherit WrapProperty<'p, 't> ()
    interface ICustomProperties with
        member this.Count = this.Target.Count

[<AbstractClass>]
type CustomProperty<'p, 'spec, 'value when 'p :> ICustomProperty and 'spec :> IPropertySpec> (owner, spec, value) =
    inherit Property<'spec, 'value> (owner, spec, value)
    // abstract members
    abstract member Self : 'p with get
    abstract member Spawn : IOwner -> Key -> 'p
    abstract member SyncTo : 'p -> unit
    // virtual members
#if FABLE_COMPILER
    [<PassGenericsAttribute>]
#endif
    abstract member SetupCloneBefore : 'p -> unit
#if FABLE_COMPILER
    [<PassGenericsAttribute>]
#endif
    abstract member SetupCloneAfter : 'p -> unit
#if FABLE_COMPILER
    [<PassGenericsAttribute>]
#endif
    default __.SetupCloneBefore (_p : 'p) = ()
#if FABLE_COMPILER
    [<PassGenericsAttribute>]
#endif
    default __.SetupCloneAfter (_p : 'p) = ()
    override __.Kind = PropertyKind.CustomProperty
    override this.AsCustom = this.Self :> ICustomProperty
    member this.AsCustomProperty = this :> ICustomProperty<'p>
    interface ICustomProperty<'p> with
        member this.Self = this.Self
        member this.SyncTo other = this.SyncTo other
#if FABLE_COMPILER
        [<PassGenericsAttribute>]
#endif
        member this.Clone o k =
            this.Spawn o k
            |> this.SetupClone (Some this.SetupCloneBefore)
            |> fun clone ->
                this.SetupCloneAfter clone
                clone

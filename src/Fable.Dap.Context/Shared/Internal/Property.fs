[<AutoOpen>]
module Dap.Context.Internal.Property

open System

open Dap.Prelude
open Dap.Context
open Dap.Context.Unsafe

[<AbstractClass>]
type UnsafeProperty internal (owner') =
    let owner : IOwner = owner'
    member __.Owner = owner
    abstract member Kind : PropertyKind with get
#if FABLE_COMPILER
    interface IUnsafeProperty
#else
    abstract member AsVar : IVarProperty with get
    abstract member AsMap : IDictProperty with get
    abstract member AsList : IListProperty with get
    abstract member AsCombo : IComboProperty with get
    abstract member AsCustom : ICustomProperty with get
    abstract member ToVar<'v1> : unit -> IVarProperty<'v1>
    abstract member ToDict<'p1 when 'p1 :> IProperty> : unit -> IDictProperty<'p1>
    abstract member ToList<'p1 when 'p1 :> IProperty> : unit -> IListProperty<'p1>
    abstract member ToCustom<'p1 when 'p1 :> ICustomProperty> : unit -> ICustomProperty<'p1>
    member this.CastFailed<'p> () : 'p = failWith (this.GetType().FullName) ("Cast_Failed: " + typeof<'p>.Name)
    default this.AsVar = this.CastFailed<IVarProperty> ()
    default this.AsMap = this.CastFailed<IDictProperty> ()
    default this.AsList = this.CastFailed<IListProperty> ()
    default this.AsCombo = this.CastFailed<IComboProperty> ()
    default this.AsCustom = this.CastFailed<ICustomProperty> ()
    default this.ToVar<'v1> () = this.CastFailed<IVarProperty<'v1>> ()
    default this.ToDict<'p1 when 'p1 :> IProperty> () = this.CastFailed<IDictProperty<'p1>> ()
    default this.ToList<'p1 when 'p1 :> IProperty> () = this.CastFailed<IListProperty<'p1>> ()
    default this.ToCustom<'p1 when 'p1 :> ICustomProperty> () = this.CastFailed<ICustomProperty<'p1>> ()
    interface IUnsafeProperty with
        member this.AsVar = this.AsVar
        member this.AsMap = this.AsMap
        member this.AsList = this.AsList
        member this.AsCombo = this.AsCombo
        member this.AsCustom = this.AsCustom
        member this.ToVar<'v1> () = this.ToVar<'v1> ()
        member this.ToDict<'p1 when 'p1 :> IProperty> () = this.ToDict<'p1> ()
        member this.ToList<'p1 when 'p1 :> IProperty> () = this.ToList<'p1> ()
        member this.ToCustom<'p1 when 'p1 :> ICustomProperty> () = this.ToCustom<'p1> ()
#endif

type IProperty with
    member this.SetupClone<'p when 'p :> IProperty> (setup : ('p -> unit) option) (clone : 'p) =
        setup
        |> Option.defaultValue this.SyncTo0
        |> fun setup ->
            setup clone
        if this.Sealed then (clone :> IProperty) .Seal ()
        clone

[<AbstractClass>]
type Property<'spec, 'value when 'spec :> IPropertySpec> internal (owner, spec', value') =
    inherit UnsafeProperty (owner)
    let spec : 'spec = spec'
    let mutable value : 'value = value'
    let mutable ver = 0
    let mutable sealed' : bool = false
    let onChanged0 : Bus<PropertyChanged> = new Bus<PropertyChanged> (owner, sprintf "%s:OnChanged0" spec.Luid)
    // abstract members
    abstract member ToJson : 'value -> Json
    abstract member DoLoadJson : 'value -> Json -> (bool * 'value option)
    abstract member Clone0 : IOwner * Key -> IProperty
    abstract member SyncTo0 : IProperty -> unit
    // virtual members
    abstract member OnSealed : unit -> unit
    abstract member ShouldSetValue : 'value -> bool
    abstract member OnValueChanged : 'value -> unit
    default __.OnSealed () = ()
    default __.ShouldSetValue (_v : 'value) = true
    default __.OnValueChanged (_v : 'value) = ()
    member __.Spec = spec
    member __.Value = value
    member __.Ver = ver
    member __.Sealed = sealed'
    member internal this.SetValue v =
        if sealed' then
            logAspectError this "SetValue:Already_Sealed" (value, v)
            false
        else
            if this.ShouldSetValue v then
                let old = value
                ver <- ver + 1
                value <- v
                this.OnValueChanged old
                if onChanged0.HasWatchers then
                    let evt0 : PropertyChanged =
                        {
                            Spec = spec :> IPropertySpec
                            Old = this.ToJson old
                            New = this.ToJson v
                        }
                    onChanged0.Trigger evt0
                true
            else
                false
    override this.ToString () =
#if FABLE_COMPILER
        sprintf "[%s:Spec=%s,Ver=%d%s]" (base.ToString ()) (spec.ToString ()) ver (if sealed' then ",Sealed" else "")
#else
        sprintf "[<%s>:Spec=%s,Ver=%d%s]" (this.GetType().FullName) (spec.ToString ()) ver (if sealed' then ",Sealed" else "")
#endif
    member this.AsProperty = this :> IProperty
    interface IProperty with
        member this.Kind = this.Kind
        member __.Spec0 = spec :> IPropertySpec
        member this.Seal () =
            if not sealed' then
                sealed' <- true
                this.OnSealed ()
        member __.Sealed = sealed'
        member this.LoadJson' json =
            if sealed' then
                logAspectError this "LoadJson':Already_Sealed" (value, E.encode 4 json)
                false
            else
                let (ok, newValue) = this.DoLoadJson value json
                newValue
                |> Option.map (fun v -> (this.SetValue v) && ok)
                |> Option.defaultValue ok
        member __.OnChanged0 = onChanged0.Publish
        member this.Clone0 (o, k) = this.Clone0 (o, k)
        member this.SyncTo0 t =
            if this.Kind <> t.Kind then
                logAspectError this "SyncTo0:InValid_Kind" t
        #if !FABLE_COMPILER
            elif this.GetType () <> t.GetType () then
                logAspectError this "SyncTo0:InValid_Kind" t
        #endif
            else
                this.SyncTo0 t
    interface IAspect with
        member __.Owner = owner
        member __.Ver = ver
        member __.SpecA = spec :> IAspectSpec
    interface IJson with
        member this.ToJson () = this.ToJson value
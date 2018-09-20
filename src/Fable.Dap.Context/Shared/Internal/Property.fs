[<AutoOpen>]
module Dap.Context.Internal.Property

open System
#if FABLE_COMPILER
open Fable.Core
#endif

open Dap.Prelude
open Dap.Context
open Dap.Context.Unsafe

let internal tplPropertyDebug = LogEvent.Template4<string, Luid, obj, obj>(LogLevelDebug, "[{Section}] {Spec} {Value} {Detail}")
let internal tplPropertyInfo = LogEvent.Template4<string, Luid, obj, obj>(LogLevelInformation, "[{Section}] {Spec} {Value} {Detail}")
let internal tplPropertyError = LogEvent.Template4<string, Luid, obj, obj>(LogLevelError, "[{Section}] {Spec} {Value} {Detail}")

[<AbstractClass>]
type UnsafeProperty internal (owner') =
    let owner : IOwner = owner'
    member __.Owner = owner
    abstract member Kind : PropertyKind with get
    abstract member AsVar : IVarProperty with get
    abstract member AsMap : IDictProperty with get
    abstract member AsList : IListProperty with get
    abstract member AsCombo : IComboProperty with get
    abstract member AsCustom : ICustomProperty with get
    abstract member ToVar<'v1> : unit -> IVarProperty<'v1>
    abstract member ToMap<'p1 when 'p1 :> IProperty> : unit -> IDictProperty<'p1>
    abstract member ToList<'p1 when 'p1 :> IProperty> : unit -> IListProperty<'p1>
    abstract member ToCustom<'p1 when 'p1 :> ICustomProperty> : unit -> ICustomProperty<'p1>
#if FABLE_COMPILER
    [<PassGenericsAttribute>]
    member this.CastFailed<'p> () : 'p = failWith "UnsafeProperty" ("Cast_Failed: " + typeof<'p>.Name)
#else
    member this.CastFailed<'p> () : 'p = failWith (this.GetType().FullName) ("Cast_Failed: " + typeof<'p>.Name)
#endif
    default this.AsVar = this.CastFailed<IVarProperty> ()
    default this.AsMap = this.CastFailed<IDictProperty> ()
    default this.AsList = this.CastFailed<IListProperty> ()
    default this.AsCombo = this.CastFailed<IComboProperty> ()
    default this.AsCustom = this.CastFailed<ICustomProperty> ()
#if FABLE_COMPILER
    [<PassGenericsAttribute>]
#endif
    default this.ToVar<'v1> () = this.CastFailed<IVarProperty<'v1>> ()
#if FABLE_COMPILER
    [<PassGenericsAttribute>]
#endif
    default this.ToMap<'p1 when 'p1 :> IProperty> () = this.CastFailed<IDictProperty<'p1>> ()
#if FABLE_COMPILER
    [<PassGenericsAttribute>]
#endif
    default this.ToList<'p1 when 'p1 :> IProperty> () = this.CastFailed<IListProperty<'p1>> ()
#if FABLE_COMPILER
    [<PassGenericsAttribute>]
#endif
    default this.ToCustom<'p1 when 'p1 :> ICustomProperty> () = this.CastFailed<ICustomProperty<'p1>> ()
    interface IUnsafeProperty with
        member this.AsVar = this.AsVar
        member this.AsMap = this.AsMap
        member this.AsList = this.AsList
        member this.AsCombo = this.AsCombo
        member this.AsCustom = this.AsCustom
#if FABLE_COMPILER
        [<PassGenericsAttribute>]
#endif
        member this.ToVar<'v1> () = this.ToVar<'v1> ()
#if FABLE_COMPILER
        [<PassGenericsAttribute>]
#endif
        member this.ToMap<'p1 when 'p1 :> IProperty> () = this.ToMap<'p1> ()
#if FABLE_COMPILER
        [<PassGenericsAttribute>]
#endif
        member this.ToList<'p1 when 'p1 :> IProperty> () = this.ToList<'p1> ()
#if FABLE_COMPILER
        [<PassGenericsAttribute>]
#endif
        member this.ToCustom<'p1 when 'p1 :> ICustomProperty> () = this.ToCustom<'p1> ()

type IProperty with
#if FABLE_COMPILER
    [<PassGenericsAttribute>]
#endif
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
    let onChanged : Bus<PropertyChanged> = new Bus<PropertyChanged> (owner, sprintf "%s:OnChanged" spec.Luid)
    // abstract members
    abstract member ToJson : 'value -> Json
    abstract member WithJson : 'value -> Json -> ('value * bool) option
    abstract member Clone0 : IOwner -> Key -> IProperty
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
            owner.Log <| tplPropertyError "Property:Already_Sealed" spec.Luid ver (value, v)
            false
        else
            if this.ShouldSetValue v then
                let old = v
                ver <- ver + 1
                value <- v
                this.OnValueChanged old
                if onChanged.HasWatchers then
                    let evt0 : PropertyChanged =
                        {
                            Spec = spec :> IPropertySpec
                            Old = this.ToJson old
                            New = this.ToJson v
                        }
                    onChanged.Trigger evt0
                true
            else
                false
    member this.AsProperty = this :> IProperty
    interface IProperty with
        member this.Kind = this.Kind
        member __.Ver = ver
        member __.Spec = spec :> IPropertySpec
        member this.Seal () =
            if not sealed' then
                sealed' <- true
                this.OnSealed ()
        member __.Sealed = sealed'
        member this.WithJson json =
            if sealed' then
                owner.Log <| tplPropertyError "Property:Already_Sealed" spec.Luid ver (value, E.encode 4 json)
                false
            else
                this.WithJson value json
                |> Option.map (fun (v, ok) ->
                    (this.SetValue v) && ok
                )|> Option.defaultValue false
        member __.OnChanged = onChanged.Publish
        member this.Clone0 o k = this.Clone0 o k
    interface IAspect with
        member __.Owner = owner
    interface IJson with
        member this.ToJson () = this.ToJson value
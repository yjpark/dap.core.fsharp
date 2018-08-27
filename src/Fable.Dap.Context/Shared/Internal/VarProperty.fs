[<AutoOpen>]
module Dap.Context.Internal.VarProperty

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

type internal VarProperty<'v> private (owner', spec') =
    let owner : IOwner = owner'
    let spec : IVarPropertySpec<'v> = spec'
    let mutable ver = 0
    let mutable value :'v = spec.InitValue
    let mutable sealed' : bool = false
    let onValueChanged : Bus<VarPropertyChanged<'v>> = new Bus<VarPropertyChanged<'v>> (owner)
    let onChanged : Bus<PropertyChanged> = new Bus<PropertyChanged> (owner)
    let toJson (v : 'v) =
        spec.Encoder v
    let setValue (v : 'v) =
        //TODO: check whether old = new
        let evt : VarPropertyChanged<'v> =
            {
                Spec = spec
                Old = value
                New = v
            }
        ver <- ver + 1
        value <- v
        onValueChanged.Trigger evt
        if onChanged.HasWatchers then
            let evt0 : PropertyChanged =
                {
                    Spec = spec :> IPropertySpec
                    Old = toJson evt.Old
                    New = toJson evt.New
                }
            onChanged.Trigger evt0
    static member Create o s = new VarProperty<'v>(o, s)
    member this.AsVarProperty = this :> IVarProperty<'v>
    member this.AsVarProperty0 = this :> IVarProperty
    member this.AsProperty = this :> IProperty
    interface IVarProperty<'v> with
        member __.Spec = spec
        member __.Value = value
        member this.SetValue v =
            match spec.Validator with
            | None ->
                setValue v
                true
            | Some validator ->
                match validator this.AsVarProperty v with
                | true ->
                    setValue v
                    true
                | false ->
                    owner.Log <| tplPropertyDebug "Property:Invalid_Value" spec.Luid value v
                    false
        member __.OnValueChanged = onValueChanged.Publish
        member this.Clone o k =
            let clone = VarProperty<'v>.Create o <| spec.ForClone k
            this.AsProperty.ToJson () |> clone.AsProperty.WithJson |> ignore
            if sealed' then clone.AsProperty.Seal ()
            clone.AsVarProperty
    interface IVarProperty with
        member __.ValueType = typeof<'v>
    interface IProperty with
        member __.Kind = PropertyKind.VarProperty
        member __.Ver = ver
        member __.Spec = spec :> IPropertySpec
        member __.Seal () =
            if not sealed' then
                sealed' <- true
        member __.Sealed = sealed'
        member this.WithJson json =
            if sealed' then
                owner.Log <| tplPropertyError "Property:Already_Sealed" spec.Luid value (E.encode 4 json)
                false
            else
                tryCastJson spec.Decoder json
                |> function
                    | Ok v ->
                        this.AsVarProperty.SetValue v
                    | Error err ->
                        owner.Log <| tplPropertyError "Property:Decode_Failed" spec.Luid value (E.encode 4 json, err)
                        false
        member this.OnChanged = onChanged.Publish
        member this.Clone0 o k = this.AsVarProperty.Clone o k :> IProperty
    interface IUnsafeProperty with
        member this.AsVar = this.AsVarProperty0
        member this.AsMap = failWith (this.GetType().FullName) "Cast_Failed"
        member this.AsList = failWith (this.GetType().FullName) "Cast_Failed"
        member this.AsCombo = failWith (this.GetType().FullName) "Cast_Failed"
        member this.AsCustom = failWith (this.GetType().FullName) "Cast_Failed"
#if FABLE_COMPILER
        [<PassGenericsAttribute>]
#endif
        member this.ToVar<'v1> () =
            if typeof<'v> = typeof<'v1> then
                this.AsVarProperty0 :?> IVarProperty<'v1>
            else
                failWith (this.GetType().FullName) <| "Cast_Failed: " + typeof<'v1>.FullName
        member this.ToMap<'p1 when 'p1 :> IProperty> () : IMapProperty<'p1> = failWith (this.GetType().FullName) "Cast_Failed"
        member this.ToList<'p1 when 'p1 :> IProperty> () : IListProperty<'p1> = failWith (this.GetType().FullName) "Cast_Failed"
        member this.ToCustom<'p1 when 'p1 :> ICustomProperty> () : ICustomProperty<'p1> = failWith (this.GetType().FullName) "Cast_Failed"
    interface IJson with
        member this.ToJson () = toJson value
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

type internal VarProperty<'v> (owner', spec') =
    let owner : IOwner = owner'
    let spec : PropertySpec<'v> = spec'
    let mutable ver = 0
    let mutable value :'v = spec.InitValue
    let mutable sealed' : bool = false
    let onChanged : Bus<PropertyChanged<'v>> = new Bus<PropertyChanged<'v>> (owner)
    let onChanged0 : Bus<PropertyChanged> = new Bus<PropertyChanged> (owner)
    let toJson (v : 'v) =
        spec.Encoder v
    let setValue (v : 'v) =
        //TODO: check whether old = new
        let evt : PropertyChanged<'v> =
            {
                Spec = spec.AsSpec
                Old = value
                New = v
            }
        ver <- ver + 1
        value <- v
        onChanged.Trigger evt
        if onChanged0.HasWatchers then
            let evt0 : PropertyChanged =
                {
                    Spec = spec.AsSpec1
                    Old = toJson evt.Old
                    New = toJson evt.New
                }
            onChanged0.Trigger evt0
    static member Create o s = new VarProperty<'v>(o, s)
    member this.AsVarProperty = this :> IVarProperty<'v>
    member this.AsVarProperty0 = this :> IVarProperty
    member this.AsProperty = this :> IProperty
    interface IVarProperty<'v> with
        member _this.Spec = spec.AsSpec
        member _this.Value = value
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
        member _this.OnChanged = onChanged.Publish
        member this.Clone o k =
            let clone = VarProperty<'v>.Create o <| spec.ForClone k
            this.AsProperty.ToJson () |> clone.AsProperty.WithJson |> ignore
            if sealed' then clone.AsProperty.Seal ()
            clone.AsVarProperty
    interface IVarProperty with
        member _this.ValueType = typeof<'v>
    interface IProperty with
        member _this.Kind = PropertyKind.VarProperty
        member _this.Ver = ver
        member _this.Spec = spec :> IPropertySpec
        member _this.Seal () =
            if not sealed' then
                sealed' <- true
        member _this.Sealed = sealed'
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
        member this.OnChanged0 = onChanged0.Publish
        member this.Clone0 o k = this.AsVarProperty.Clone o k :> IProperty
    interface IUnsafeProperty with
        member this.AsVar = this.AsVarProperty0
        member this.AsMap = failWith (this.GetType().FullName) "Cast_Failed"
        member this.AsList = failWith (this.GetType().FullName) "Cast_Failed"
        member this.AsCombo = failWith (this.GetType().FullName) "Cast_Failed"
#if FABLE_COMPILER
        [<PassGenericsAttribute>]
#endif
        member this.ToVar<'v1> () =
            if typeof<'v> = typeof<'v1> then
                this.AsVarProperty0 :?> IVarProperty<'v1>
            else
                failWith (this.GetType().FullName) <| "Cast_Failed: " + typeof<'v1>.FullName
        member this.ToMap<'v1> () : IMapProperty<'v1> = failWith (this.GetType().FullName) "Cast_Failed"
        member this.ToList<'v1> () : IListProperty<'v1> = failWith (this.GetType().FullName) "Cast_Failed"
    interface IJson with
        member this.ToJson () = toJson value
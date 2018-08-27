[<AutoOpen>]
module Dap.Context.Internal.Property

open System
#if FABLE_COMPILER
open Fable.Core
#endif

open Dap.Prelude
open Dap.Context

let internal tplPropertyDebug = LogEvent.Template4<string, Luid, obj, obj>(LogLevelDebug, "[{Section}] {Spec} {Value} {Detail}")
let internal tplPropertyInfo = LogEvent.Template4<string, Luid, obj, obj>(LogLevelInformation, "[{Section}] {Spec} {Value} {Detail}")
let internal tplPropertyError = LogEvent.Template4<string, Luid, obj, obj>(LogLevelError, "[{Section}] {Spec} {Value} {Detail}")

type internal Property<'v> (owner', spec') =
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
    static member Create o s = new Property<'v>(o, s)
    member this.AsProperty = this :> IProperty<'v>
    member this.AsProperty0 = this :> IProperty
    interface IProperty<'v> with
        member _this.Spec = spec.AsSpec
        member _this.Value = value
        member this.SetValue v =
            match spec.Validator with
            | None ->
                setValue v
                true
            | Some validator ->
                match validator this.AsProperty v with
                | true ->
                    setValue v
                    true
                | false ->
                    owner.Log <| tplPropertyDebug "Property:Invalid_Value" spec.Luid value v
                    false
        member _this.OnChanged = onChanged.Publish
    interface IProperty with
        member _this.Ver = ver
        member _this.Spec = spec :> IPropertySpec
        member _this.Seal () =
            if not sealed' then
                sealed' <- true
                true
            else
                false
        member _this.Sealed = sealed'
        member this.WithJson json =
            if sealed' then
                owner.Log <| tplPropertyError "Property:Already_Sealed" spec.Luid value (E.encode 4 json)
                false
            else
                tryCastJson spec.Decoder json
                |> function
                    | Ok v ->
                        this.AsProperty.SetValue v
                    | Error err ->
                        owner.Log <| tplPropertyError "Property:Decode_Failed" spec.Luid value (E.encode 4 json, err)
                        false
        member this.OnChanged0 = onChanged0.Publish
    interface IJson with
        member this.ToJson () = toJson value
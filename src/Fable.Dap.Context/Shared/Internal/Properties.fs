[<AutoOpen>]
module Dap.Context.Internal.Properties

open System
#if FABLE_COMPILER
open Fable.Core
#endif

open Dap.Prelude
open Dap.Context
open Dap.Context.Internal

type internal Properties (owner', spec') =
    let owner : IOwner = owner'
    let spec : PropertySpec = spec'
    let mutable value : Map<Key, IProperty> = Map.empty
    let mutable sealed' : bool = false
    let mutable comboSealed' : bool = false
    let onAdded = new Bus<IProperty> (owner)
    let onChanged0 = new Bus<PropertyChanged> (owner)
#if FABLE_COMPILER
    [<PassGenericsAttribute>]
#endif
    let toJson (v : Map<Key, IProperty>) =
        v
        |> Map.toList
        |> List.map (fun (k, prop) ->
            k, prop.ToJson ()
        )|> E.object
    let checkAdd (subSpec : IPropertySpec) (subType : Type) =
        if comboSealed' then
            failWith "Combo_Sealed" <| sprintf "[%s] <%s> [%s]" spec.Luid subType.FullName subSpec.Key
        value
        |> Map.tryFind subSpec.Key
        |> Option.iter (fun prop ->
            failWith "Key_Exist" <| sprintf "[%s] <%s> [%s] %A -> %A" spec.Luid subType.FullName subSpec.Luid prop subSpec
        )
    let add (prop : 'prop when 'prop :> IProperty) =
        let k = (prop :> IProperty) .Spec.Key
        let oldJson =
            if onChanged0.HasWatchers then
                Some <| toJson value
            else
                None
        value <-
            value
            |> Map.add k (prop :> IProperty)
        onAdded.Trigger prop
        oldJson
        |> Option.iter (fun oldJson ->
            let evt0 : PropertyChanged =
                {
                    Spec = spec.AsSpec
                    Old = oldJson
                    New = toJson value
                }
            onChanged0.Trigger evt0
        )
        prop
    static member Create o s = new Properties(o, s)
    member this.AsProperties = this :> IProperties
    member this.IProperties = this :> IProperty
    interface IProperties with
        member _this.Value = value
        member _this.SealCombo () =
            if not comboSealed' then
                comboSealed' <- true
                true
            else
                false
        member _this.ComboSealed = comboSealed'
        member _this.TryGet k = value |> Map.tryFind k
        member this.Has k =
            (this.AsProperties.TryGet k).IsSome
        member this.Get k =
            this.AsProperties.TryGet k
            |> function
                | Some prop -> prop
                | None -> failWith "Not_Found" k
        member _this.Add (prop : IProperty) =
            checkAdd prop.Spec (prop.GetType())
            add prop |> ignore
#if FABLE_COMPILER
        [<PassGenericsAttribute>]
#endif
        member _this.Add<'v> (subSpec : IPropertySpec<'v>) : IProperty<'v> =
            checkAdd subSpec typeof<'v>
            PropertySpec<'v>.AsSubSpec' subSpec spec
            |> Property<'v>.Create owner
            |> add
            |> fun prop -> prop.AsProperty
#if FABLE_COMPILER
        [<PassGenericsAttribute>]
#endif
        member _this.AddMap<'v> (subSpec : IPropertySpec<'v>) : IPropertyMap<'v> =
            checkAdd subSpec typeof<'v>
            PropertySpec<'v>.AsSubSpec' subSpec spec
            |> PropertyMap<'v>.Create owner
            |> add
            |> fun prop -> prop.AsPropertyMap
#if FABLE_COMPILER
        [<PassGenericsAttribute>]
#endif
        member _this.AddList<'v> (subSpec : IPropertySpec<'v>) : IPropertyList<'v> =
            checkAdd subSpec typeof<'v>
            PropertySpec<'v>.AsSubSpec' subSpec spec
            |> PropertyList<'v>.Create owner
            |> add
            |> fun prop -> prop.AsPropertyList
        member _this.OnAdded = onAdded.Publish
    interface IProperty with
        member _this.Ver = value.Count
        member _this.Spec = spec.AsSpec
        member _this.Seal () =
            if not sealed' then
                sealed' <- true
                true
            else
                false
        member _this.Sealed = sealed'
        member _this.WithJson json =
            if sealed' then
                owner.Log <| tplPropertyError "Property:Already_Sealed" spec.Luid value (E.encode 4 json)
                false
            else
                let mutable ok = true
                value
                |> Map.toList
                |> List.iter (fun (k, prop) ->
                    match tryCastJson (D.field k D.value) json with
                    | Ok json ->
#if FABLE_COMPILER
                        let json = fableObjToJson json
#endif
                        let oneOk = prop.WithJson json
                        if not oneOk then
                            ok <- false
                    | Error err ->
                        ok <- false
                        owner.Log <| tplPropertyError "Properties:Decode_Field_Failed" k (prop.ToJson ()) err
                )
                if not ok then
                    logError owner "Properties:WithJson" "Decode_Has_Error" (E.encode 4 json)
                ok
        member _this.OnChanged0 = onChanged0.Publish
    interface IJson with
        member this.ToJson () = toJson value

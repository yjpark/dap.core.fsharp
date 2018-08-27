[<AutoOpen>]
module Dap.Context.Internal.ComboProperty

open System
#if FABLE_COMPILER
open Fable.Core
#endif

open Dap.Prelude
open Dap.Context
open Dap.Context.Unsafe
open Dap.Context.Internal

type internal ComboProperty (owner', spec') =
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
    static member Create o s = new ComboProperty(o, s)
    member this.AsComboProperty = this :> IComboProperty
    member this.AsProperty = this :> IProperty
    member this.AsProperties = this :> IProperties
    interface IComboProperty with
        member _this.Value = value
        member _this.SealCombo () =
            if not comboSealed' then
                comboSealed' <- true
        member _this.ComboSealed = comboSealed'
        member _this.TryGet k = value |> Map.tryFind k
        member this.Has k =
            (this.AsComboProperty.TryGet k).IsSome
        member this.Get k =
            this.AsComboProperty.TryGet k
            |> function
                | Some prop -> prop
                | None -> failWith "Not_Found" k
#if FABLE_COMPILER
        [<PassGenericsAttribute>]
#endif
        member _this.AddVar<'v> (subSpec : IPropertySpec<'v>) =
            checkAdd subSpec typeof<'v>
            PropertySpec<'v>.AsSubSpec' subSpec spec
            |> VarProperty<'v>.Create owner
            |> add
            |> fun prop -> prop.AsVarProperty
#if FABLE_COMPILER
        [<PassGenericsAttribute>]
#endif
        member _this.AddMap<'v> (subSpec : IPropertySpec<'v>) =
            checkAdd subSpec typeof<'v>
            PropertySpec<'v>.AsSubSpec' subSpec spec
            |> MapProperty<'v>.Create owner
            |> add
            |> fun prop -> prop.AsMapProperty
#if FABLE_COMPILER
        [<PassGenericsAttribute>]
#endif
        member _this.AddList<'v> (subSpec : IPropertySpec<'v>) =
            checkAdd subSpec typeof<'v>
            PropertySpec<'v>.AsSubSpec' subSpec spec
            |> ListProperty<'v>.Create owner
            |> add
            |> fun prop -> prop.AsListProperty
        member _this.AddCombo (subSpec : IPropertySpec) =
            checkAdd subSpec typeof<IComboProperty>
            PropertySpec.AsSubSpec' subSpec spec
            |> ComboProperty.Create owner
            |> add
            |> fun prop -> prop.AsComboProperty
#if FABLE_COMPILER
        [<PassGenericsAttribute>]
#endif
        member _this.AddCustom<'p when 'p :> IProperty> (key : Key) (spawner : PropertySpawner<'p>) =
            let prop = spawner owner key
            checkAdd prop.Spec (prop.GetType())
            add prop
        member _this.AddCustom0 (key : Key) (spawner : PropertySpawner) =
            let prop = spawner owner key
            checkAdd prop.Spec (prop.GetType())
            add prop
        member _this.OnAdded = onAdded.Publish
        member this.Clone o k =
            let clone = ComboProperty.Create o <| spec.ForClone k
            value
            |> Map.toList
            |> List.iter (fun (key, prop) ->
                clone.AsComboProperty.AddCustom0 key prop.Clone0 |> ignore
            )
            this.AsProperty.ToJson () |> clone.AsProperty.WithJson |> ignore
            if sealed' then clone.AsProperty.Seal ()
            if comboSealed' then clone.AsComboProperty.SealCombo () |> ignore
            clone.AsComboProperty
    interface IProperties with
        member this.Clone1 o k = this.AsComboProperty.Clone o k :> IProperties
    interface IProperty with
        member _this.Kind = PropertyKind.ComboProperty
        member _this.Ver = value.Count
        member _this.Spec = spec.AsSpec
        member _this.Seal () =
            if not sealed' then
                sealed' <- true
                comboSealed' <- true
                value
                |> Map.toList
                |> List.iter (fun (_key, prop) ->
                    prop.Seal ()
                )
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
                        owner.Log <| tplPropertyError "ComboProperty:Decode_Field_Failed" k (prop.ToJson ()) err
                )
                if not ok then
                    logError owner "ComboProperty:WithJson" "Decode_Has_Error" (E.encode 4 json)
                ok
        member _this.OnChanged0 = onChanged0.Publish
        member this.Clone0 o k = this.AsComboProperty.Clone o k :> IProperty
    interface IUnsafeProperty with
        member this.AsVar = failWith (this.GetType().FullName) "Cast_Failed"
        member this.AsMap = failWith (this.GetType().FullName) "Cast_Failed"
        member this.AsList = failWith (this.GetType().FullName) "Cast_Failed"
        member this.AsCombo = this.AsComboProperty
        member this.ToVar<'v1> () : IVarProperty<'v1> = failWith (this.GetType().FullName) "Cast_Failed"
        member this.ToMap<'v1> () : IMapProperty<'v1> = failWith (this.GetType().FullName) "Cast_Failed"
        member this.ToList<'v1> () : IListProperty<'v1> = failWith (this.GetType().FullName) "Cast_Failed"
    interface IJson with
        member this.ToJson () = toJson value

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
    let spec : IPropertySpec = spec'
    let mutable value : Map<Key, IProperty> = Map.empty
    let mutable sealed' : bool = false
    let mutable comboSealed' : bool = false
    let onAdded = new Bus<IProperty> (owner)
    let onChanged = new Bus<PropertyChanged> (owner)
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
            if onChanged.HasWatchers then
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
                    Spec = spec
                    Old = oldJson
                    New = toJson value
                }
            onChanged.Trigger evt0
        )
        prop
    static member Create o s = new ComboProperty(o, s)
    member this.AsComboProperty = this :> IComboProperty
    member this.AsProperty = this :> IProperty
    member this.AsProperties = this :> IProperties
    interface IComboProperty with
        member __.Value = value
        member __.SealCombo () =
            if not comboSealed' then
                comboSealed' <- true
        member __.ComboSealed = comboSealed'
        member __.TryGet k = value |> Map.tryFind k
        member this.Has k =
            (this.AsComboProperty.TryGet k).IsSome
        member this.Get k =
            this.AsComboProperty.TryGet k
            |> function
                | Some prop -> prop
                | None -> failWith "Not_Found" k
        member __.AddAny (key : Key) (spawner : PropertySpawner) =
            let prop = spawner owner key
            checkAdd prop.Spec (prop.GetType())
            add prop
#if FABLE_COMPILER
        [<PassGenericsAttribute>]
#endif
        member __.AddVar<'v> (subSpec : IVarPropertySpec<'v>) =
            checkAdd subSpec typeof<'v>
            VarPropertySpec<'v>.AsSubSpec' subSpec spec
            |> VarProperty<'v>.Create owner
            |> add
            |> fun prop -> prop.AsVarProperty
#if FABLE_COMPILER
        [<PassGenericsAttribute>]
#endif
        member __.AddMap<'p when 'p :> IProperty> (subSpec : IPropertySpec<'p>) =
            checkAdd subSpec typeof<'p>
            subSpec.AsSubSpec spec
            |> MapProperty<'p>.Create owner
            |> add
            |> fun prop -> prop.AsMapProperty
#if FABLE_COMPILER
        [<PassGenericsAttribute>]
#endif
        member __.AddList<'p when 'p :> IProperty> (subSpec : IPropertySpec<'p>) =
            checkAdd subSpec typeof<'p>
            subSpec.AsSubSpec spec
            |> ListProperty<'p>.Create owner
            |> add
            |> fun prop -> prop.AsListProperty
        member __.AddCombo (subSpec : IPropertySpec) =
            checkAdd subSpec typeof<IComboProperty>
            subSpec.AsSubSpec spec
            |> ComboProperty.Create owner
            |> add
            |> fun prop -> prop.AsComboProperty
#if FABLE_COMPILER
        [<PassGenericsAttribute>]
#endif
        member __.AddCustom<'p when 'p :> ICustomProperty> (subSpec : IPropertySpec<'p>) =
            checkAdd subSpec typeof<'p>
            subSpec.AsSubSpec spec
            |> fun spec -> spec.Spawn owner
            |> add
        member __.AddCustom0 (subSpec : IPropertySpec<ICustomProperty>) =
            checkAdd subSpec typeof<ICustomProperty>
            subSpec.AsSubSpec spec
            |> fun spec -> spec.Spawn owner
            |> add
        member __.OnAdded = onAdded.Publish
        member this.Clone o k =
            let clone = ComboProperty.Create o <| spec.ForClone k
            value
            |> Map.toList
            |> List.iter (fun (key, prop) ->
                clone.AsComboProperty.AddAny key prop.Clone0 |> ignore
            )
            this.AsProperty.ToJson () |> clone.AsProperty.WithJson |> ignore
            if sealed' then clone.AsProperty.Seal ()
            if comboSealed' then clone.AsComboProperty.SealCombo () |> ignore
            clone.AsComboProperty
    interface IProperties with
        member this.Clone1 o k = this.AsComboProperty.Clone o k :> IProperties
    interface IProperty with
        member __.Kind = PropertyKind.ComboProperty
        member __.Ver = value.Count
        member __.Spec = spec
        member __.Seal () =
            if not sealed' then
                sealed' <- true
                comboSealed' <- true
                value
                |> Map.toList
                |> List.iter (fun (_key, prop) ->
                    prop.Seal ()
                )
        member __.Sealed = sealed'
        member __.WithJson json =
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
        member __.OnChanged = onChanged.Publish
        member this.Clone0 o k = this.AsComboProperty.Clone o k :> IProperty
    interface IUnsafeProperty with
        member this.AsVar = failWith (this.GetType().FullName) "Cast_Failed"
        member this.AsMap = failWith (this.GetType().FullName) "Cast_Failed"
        member this.AsList = failWith (this.GetType().FullName) "Cast_Failed"
        member this.AsCombo = this.AsComboProperty
        member this.AsCustom = failWith (this.GetType().FullName) "Cast_Failed"
        member this.ToVar<'v1> () : IVarProperty<'v1> = failWith (this.GetType().FullName) "Cast_Failed"
        member this.ToMap<'p1 when 'p1 :> IProperty> () : IMapProperty<'p1> = failWith (this.GetType().FullName) "Cast_Failed"
        member this.ToList<'p1 when 'p1 :> IProperty> () : IListProperty<'p1> = failWith (this.GetType().FullName) "Cast_Failed"
        member this.ToCustom<'p1 when 'p1 :> ICustomProperty> () : ICustomProperty<'p1> = failWith (this.GetType().FullName) "Cast_Failed"
    interface IJson with
        member this.ToJson () = toJson value

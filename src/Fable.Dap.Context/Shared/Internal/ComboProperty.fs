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

type internal ComboProperty (owner, spec) =
    inherit Property<IPropertySpec, Map<Key, IProperty>> (owner, spec, Map.empty)
    let mutable comboSealed : bool = false
    let onAdded = new Bus<IProperty> (owner, sprintf "%s:OnAdded" spec.Luid)
#if FABLE_COMPILER
    [<PassGenericsAttribute>]
#endif
    static member Create o s = new ComboProperty(o, s)
    override __.Kind = PropertyKind.ComboProperty
    override this.AsCombo = this :> IComboProperty
    member this.AsProperties = this :> IProperties
    override __.ToJson (v : Map<Key, IProperty>) =
        v
        |> Map.toList
        |> List.map (fun (k, prop) ->
            k, prop.ToJson ()
        )|> E.object
    override this.WithJson value json =
        let mutable ok = true
        value
        |> Map.iter (fun k prop ->
            match tryCastJson (D.field k D.value) json with
            | Ok propJson ->
#if FABLE_COMPILER
                let propJson = fableObjToJson propJson
#endif
                let oneOk = prop.WithJson propJson
                if not oneOk then
                    ok <- false
            | Error err ->
                ok <- false
                owner.Log <| tplPropertyError "ComboProperty:Decode_Field_Failed" k (prop.ToJson ()) err
        )
        if not ok then
            logError owner "ComboProperty:WithJson" "Decode_Has_Error" (E.encode 4 json)
        Some (value, ok)
    override this.Clone0 o k = this.AsCombo.Clone o k :> IProperty
    override this.OnSealed () =
        comboSealed <- true
        this.Value
        |> Map.iter (fun _key prop ->
            prop.Seal ()
        )
    member private this.CheckAdd (subSpec : IPropertySpec) (subType : Type) =
        if comboSealed then
            failWith "Combo_Sealed" <| sprintf "[%s] <%s> [%s]" spec.Luid subType.FullName subSpec.Key
        this.Value
        |> Map.tryFind subSpec.Key
        |> Option.iter (fun prop ->
            failWith "Key_Exist" <| sprintf "[%s] <%s> [%s] %A -> %A" spec.Luid subType.FullName subSpec.Luid prop subSpec
        )
#if FABLE_COMPILER
    [<PassGenericsAttribute>]
#endif
    member private this.Add<'prop when 'prop :> IProperty> (prop : 'prop) =
        let k = (prop :> IProperty) .Spec.Key
        if (this.Value
            |> Map.add k (prop :> IProperty)
            |> this.SetValue) then
            onAdded.Trigger prop
            prop
        else
            failWith "Add_Failed" <| sprintf "[%s] <%s> [%s]" spec.Luid (typeof<'prop>).FullName prop.Spec.Key
    interface IComboProperty with
        member __.SealCombo () =
            if not comboSealed then
                comboSealed <- true
        member __.ComboSealed = comboSealed
        member this.Value = this.Value
        member this.TryGet k = this.Value |> Map.tryFind k
        member this.Has k =
            (this.AsCombo.TryGet k).IsSome
        member this.Get k =
            this.AsCombo.TryGet k
            |> function
                | Some prop -> prop
                | None -> failWith "Not_Found" k
        member this.AddAny (key : Key) (spawner : PropertySpawner) =
            let prop = spawner owner key
            this.CheckAdd prop.Spec (prop.GetType())
            this.Add prop
#if FABLE_COMPILER
        [<PassGenericsAttribute>]
#endif
        member this.AddVar<'v> (subSpec : IVarPropertySpec<'v>) =
            this.CheckAdd subSpec typeof<'v>
            subSpec.AsSubSpec spec
            |> VarProperty<'v>.Create owner
            |> this.Add
            |> fun prop -> prop.AsVarProperty
#if FABLE_COMPILER
        [<PassGenericsAttribute>]
#endif
        member this.AddMap<'p when 'p :> IProperty> (subSpec : IPropertySpec<'p>) =
            this.CheckAdd subSpec typeof<'p>
            subSpec.AsSubSpec spec
            |> DictProperty<'p>.Create owner
            |> this.Add
            |> fun prop -> prop.AsDictProperty
#if FABLE_COMPILER
        [<PassGenericsAttribute>]
#endif
        member this.AddList<'p when 'p :> IProperty> (subSpec : IPropertySpec<'p>) =
            this.CheckAdd subSpec typeof<'p>
            subSpec.AsSubSpec spec
            |> ListProperty<'p>.Create owner
            |> this.Add
            |> fun prop -> prop.AsListProperty
        member this.AddCombo (subSpec : IPropertySpec) =
            this.CheckAdd subSpec typeof<IComboProperty>
            subSpec.AsSubSpec spec
            |> ComboProperty.Create owner
            |> this.Add
            |> fun prop -> prop.AsCombo
#if FABLE_COMPILER
        [<PassGenericsAttribute>]
#endif
        member this.AddCustom<'p when 'p :> ICustomProperty> (subSpec : IPropertySpec<'p>) =
            this.CheckAdd subSpec typeof<'p>
            subSpec.AsSubSpec spec
            |> fun spec -> spec.Spawn owner
            |> this.Add
        member __.OnAdded = onAdded.Publish
        member this.SyncTo (other : IComboProperty) =
            this.Value
            |> Map.toList
            |> List.iter (fun (key, prop) ->
                other.AddAny key prop.Clone0 |> ignore
            )
            if comboSealed
                then other.SealCombo ()
        member this.Clone o k =
            spec.ForClone k
            |> ComboProperty.Create o
            |> this.SetupClone (Some this.AsCombo.SyncTo)
            :> IComboProperty
    interface IProperties with
        member this.Count = this.Value.Count
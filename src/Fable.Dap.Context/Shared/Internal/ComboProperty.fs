[<AutoOpen>]
module Dap.Context.Internal.ComboProperty

open System

open Dap.Prelude
open Dap.Context
open Dap.Context.Unsafe
open Dap.Context.Internal

type internal ComboProperty (owner, spec) =
    inherit Property<IPropertySpec, IProperty list> (owner, spec, [])
    let mutable comboSealed : bool = false
    let onAdded = new Bus<IProperty> (owner, sprintf "%s:OnAdded" spec.Luid)
    static member Create (o, s) = new ComboProperty(o, s)
    override __.Kind = PropertyKind.ComboProperty
#if FABLE_COMPILER
    member this.AsCombo = this :> IComboProperty
#else
    override this.AsCombo = this :> IComboProperty
#endif
    member this.AsProperties = this :> IProperties
    override __.ToJson (v : IProperty list) =
        v
        |> List.map (fun prop ->
            prop.Key, prop.ToJson ()
        )|> E.object
    override this.DoLoadJson value json =
        let mutable failedProps : IProperty list = []
        value
        |> List.iter (fun prop ->
            match tryCastJson (D.field prop.Key D.json) json with
            | Ok propJson ->
                if not (prop.LoadJson' propJson) then
                    failedProps <- prop :: failedProps
            | Error err ->
                failedProps <- prop :: failedProps
                logAspectError prop "DoLoadJson:Decode_Field_Failed" err
        )
        let ok = failedProps.Length = 0
        if not ok then
            logAspectError this "DoLoadJson:Decode_Got_Error" (E.encode 4 json)
            logAspectError this (sprintf "DoLoadJson:Failed_Properties: [%d]" failedProps.Length)
                (failedProps |> List.map (fun p -> (p.Spec0, p)))
        (ok, None)
    override this.Clone0 (o, k) = this.AsCombo.Clone (o, k) :> IProperty
    override this.SyncTo0 t = this.AsCombo.SyncTo (t :?> IComboProperty)
    override this.OnSealed () =
        comboSealed <- true
        this.Value
        |> List.iter (fun prop ->
            prop.Seal ()
        )
    member private this.CheckAdd (subSpec : IPropertySpec) (subTypeName : string) =
        if comboSealed then
            failWith "Combo_Sealed" <| sprintf "[%s] <%s> [%s]" spec.Luid subTypeName subSpec.Key
        this.Value
        |> List.tryFind (fun prop -> prop.Key = subSpec.Key)
        |> Option.iter (fun prop ->
            failWith "Key_Exist" <| sprintf "[%s] <%s> [%s] -> %A" spec.Luid subTypeName subSpec.Luid prop
        )
    member private this.Add<'prop when 'prop :> IProperty> (prop : 'prop) =
        if (this.Value @ [prop :> IProperty]
            |> this.SetValue) then
            onAdded.Trigger prop
            prop
        else
            failWith "Add_Failed" <| sprintf "[%s] <%s> [%s]" spec.Luid (typeNameOf<'prop> ()) prop.Key
    interface IComboProperty with
        member __.SealCombo () =
            if not comboSealed then
                comboSealed <- true
        member __.ComboSealed = comboSealed
        member this.Value = this.Value
        member this.TryGet k =
            this.Value
            |> List.tryFind (fun prop -> k = prop.Key)
        member this.Has k =
            (this.AsCombo.TryGet k).IsSome
        member this.Get k =
            this.AsCombo.TryGet k
            |> function
                | Some prop -> prop
                | None -> failWith ("IComboProperty:Not_Found:" + k) this
        member this.AddAny (key : Key) (spawner : PropertySpawner) =
            let prop = spawner (owner, key)
            this.CheckAdd prop.Spec0 (getTypeName prop)
            this.Add prop
        member this.AddVar<'v> (subSpec : IVarPropertySpec<'v>) =
            this.CheckAdd subSpec (typeNameOf<'v> ())
            VarProperty<'v>.Create (owner, subSpec.AsSubSpec spec)
            |> this.Add
            |> fun prop -> prop.AsVarProperty
        member this.AddDict<'p when 'p :> IProperty> (subSpec : IPropertySpec<'p>) =
            this.CheckAdd subSpec (typeNameOf<'p> ())
            DictProperty<'p>.Create (owner, subSpec.AsSubSpec spec)
            |> this.Add
            |> fun prop -> prop.AsDictProperty
        member this.AddList<'p when 'p :> IProperty> (subSpec : IPropertySpec<'p>) =
            this.CheckAdd subSpec (typeNameOf<'p> ())
            ListProperty<'p>.Create (owner,  subSpec.AsSubSpec spec)
            |> this.Add
            |> fun prop -> prop.AsListProperty
        member this.AddCombo (subSpec : IPropertySpec) =
            this.CheckAdd subSpec (typeNameOf<IComboProperty> ())
            ComboProperty.Create (owner, subSpec.AsSubSpec spec)
            |> this.Add
            |> fun prop -> prop.AsCombo
        member this.AddCustom<'p when 'p :> ICustomProperty> (subSpec : IPropertySpec<'p>) =
            this.CheckAdd subSpec (typeNameOf<'p> ())
            let subSpec = subSpec.AsSubSpec spec
            subSpec.Spawn owner
            |> this.Add
        member __.OnAdded = onAdded.Publish
        member this.SyncTo (other : IComboProperty) =
            this.Value
            |> List.iter (fun prop ->
                match other.TryGet prop.Key with
                | None ->
                    other.AddAny prop.Key prop.Clone0 |> ignore
                | Some t ->
                    prop.SyncTo0 t
            )
            if comboSealed
                then other.SealCombo ()
        member this.Clone (o, k) =
            ComboProperty.Create (o,  spec.ForClone k)
            |> this.SetupClone (Some this.AsCombo.SyncTo)
            :> IComboProperty
    interface IProperties with
        member this.Count = this.Value.Length
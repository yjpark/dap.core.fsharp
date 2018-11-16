[<AutoOpen>]
module Dap.Context.Internal.ListProperty

open System

open Dap.Prelude
open Dap.Context
open Dap.Context.Unsafe
open Dap.Context.Internal

let private newSubKey () = newGuid ()

type internal ListProperty<'p when 'p :> IProperty> private (owner, spec) =
    inherit Property<IPropertySpec<'p>, 'p list> (owner, spec, [])
    let mutable listSealed : bool = false
    let onMoved = new Bus<PropertyMoved> (owner, sprintf "%s:OnMoved" spec.Luid)
    let onAdded = new Bus<'p * Index> (owner, sprintf "%s:OnAdded" spec.Luid)
    let onRemoved = new Bus<'p * Index> (owner, sprintf "%s:OnRemoved" spec.Luid)
    let onAdded0 = new Bus<IProperty * Index> (owner, sprintf "%s:OnAdded0" spec.Luid)
    let onRemoved0 = new Bus<IProperty * Index> (owner, sprintf "%s:OnRemoved0" spec.Luid)
    let spawnProp () =
        let k = newSubKey ()
        let subSpec = spec.GetSubSpec k
        (k, subSpec.Spawner (owner, k))
    let triggerMoved (prop : 'p, oldIndex : Index, newIndex : Index) =
        let evt : PropertyMoved =
            {
                Spec = prop.Spec0
                Old = oldIndex
                New = newIndex
            }
        onMoved.Trigger (evt)
    let triggerAdded (prop : 'p, index : Index) =
        onAdded.Trigger (prop, index)
        onAdded0.Trigger (prop :> IProperty, index)
    let triggerRemoved (prop : 'p, index : Index) =
        onRemoved.Trigger (prop, index)
        onRemoved0.Trigger (prop :> IProperty, index)
    static member Create (o, s : IPropertySpec<'p>) = new ListProperty<'p>(o, s)
    override __.Kind = PropertyKind.ListProperty
#if FABLE_COMPILER
    member this.AsList = this :> IListProperty
#else
    override this.AsList = this :> IListProperty
#endif
    member this.AsListProperty = this :> IListProperty<'p>
    member this.AsProperties = this :> IProperties
    override __.ToJson (props : 'p list) =
        props
        |> List.map (fun p -> p.ToJson ())
        |> E.jsonList
    override this.DoLoadJson value json =
        if json.IsArray then
            let mutable failedProps : 'p list = []
            let oldValue = this.Value
            let newValue : 'p list =
                json.ToArrayValue ()
                |> Seq.map (fun propJson ->
                    let (_k, prop) = spawnProp ()
                    if not (prop.LoadJson' propJson) then
                        failedProps <- prop :: failedProps
                    prop
                )|> Seq.toList
            let ok = failedProps.Length = 0
            if not ok then
                logAspectError this "DoLoadJson:Decode_Got_Error" (E.encode 4 json)
                logAspectError this (sprintf "DoLoadJson:Failed_Properties: [%d]" failedProps.Length)
                    (failedProps |> List.map (fun p -> (p.Spec0, p)))
            if (this.SetValue newValue) then
                oldValue |> List.iteri (fun i prop -> triggerRemoved (prop, i))
                newValue |> List.iteri (fun i prop -> triggerAdded (prop, i))
                (ok, None)
            else
                (false, None)
        else
            (false, None)
    override this.Clone0 (o, k) = this.AsListProperty.Clone (o, k) :> IProperty
    override this.SyncTo0 t = this.AsListProperty.SyncTo (t :?> IListProperty<'p>)
#if !FABLE_COMPILER
    override this.ToList<'p1 when 'p1 :> IProperty> () =
        if typeof<'p> = typeof<'p1> then
            this.AsList :?> IListProperty<'p1>
        else
            this.CastFailed<IListProperty<'p1>> ()
#endif
    override this.OnSealed () =
        listSealed <- true
        this.Value
        |> List.iter (fun prop ->
            prop.Seal ()
        )

    member private this.CheckChange tip =
        if listSealed then
            failWith "Already_Sealed" <| sprintf "[%s] <%s> [%d] %s" spec.Luid (typeNameOf<'p> ()) this.Value.Length tip
    member private this.Insert (k : Key) (prop : 'p) (toIndex : ToIndex option) : Index =
        match toIndex with
        | None ->
            let index = this.Value.Length
            let newValue = this.Value @ [prop]
            if not (this.SetValue newValue) then
                failWith "Add_Failed" <| sprintf "[%s] <%s> [%d] %s" spec.Luid (typeNameOf<'p> ()) this.Value.Length prop.Key
            index
        | Some toIndex ->
            let (before, after) = this.Value |> List.splitAt toIndex
            let newValue = before @ (prop :: after)
            if not (this.SetValue newValue) then
                failWith "Add_Failed" <| sprintf "%s [%d/%d] %s" (this.ToString ()) toIndex this.Value.Length (prop.ToString ())
            toIndex
    member private this.Add (toIndex : ToIndex option) =
        let (k, prop) = spawnProp ()
        let index = this.Insert k prop toIndex
        triggerAdded (prop, index)
        prop
    member private this.Remove (i : Index, ?triggerEvent : bool) =
        let (before, after) = this.Value |> List.splitAt i
        let prop = List.head after
        let newValue = before @ (List.tail after)
        if not (this.SetValue newValue) then
            failWith "Remove_Failed" <| sprintf "%s [%d] %s" (this.ToString ()) i (prop.ToString ())
        if (defaultArg triggerEvent true) then
            triggerRemoved (prop, i)
        prop
    member private this.CheckIndex (i : Index) =
        if i < 0 || i >= this.Value.Length then
            failWith "Invalid_Index" <| sprintf "[%s] <%s> [%d] -> [%d]" spec.Luid (typeNameOf<'p> ()) this.Value.Length i
    interface IListProperty<'p> with
        member this.Value = this.Value
        member __.Spec = spec
        member this.TryGet i =
            if i >= 0 && i < this.Value.Length then
                Some <| List.item i this.Value
            else
                None
        member this.Get i =
            this.CheckIndex i
            List.item i this.Value
        member this.Add () =
            this.CheckChange "Add"
            this.Add None
        member this.Insert i =
            this.CheckChange <| sprintf "Insert: %d" i
            if i = this.Value.Length then
                this.AsListProperty.Add ()
            else
                this.CheckIndex i
                this.Add <| Some i
        member this.Remove i =
            this.CheckChange <| sprintf "Remove: %d" i
            this.CheckIndex i
            this.Remove i
        member this.Clear' () =
            this.CheckChange "Clear"
            if this.Value.Length = 0 then
                []
            else
                let oldValue = this.Value
                if (this.SetValue []) then
                    oldValue
                    |> List.iteri (fun i prop ->
                        triggerRemoved (prop, i)
                    )
                    oldValue
                else
                    failWith "Clear_Failed" <| sprintf "[%s] <%s> [%d]" spec.Luid (typeNameOf<'p> ()) this.Value.Length
        member __.OnAdded = onAdded.Publish
        member __.OnRemoved = onRemoved.Publish
        member this.SyncTo other =
            other.Clear ()
            this.Value
            |> List.iter (fun prop ->
                prop.SyncTo0 <| other.Add ()
            )
            if listSealed
                then other.SealList ()
        member this.Clone (o, k) =
            ListProperty<'p>.Create (o, spec.ForClone k)
            |> this.SetupClone (Some this.AsListProperty.SyncTo)
            |> fun clone ->
                if listSealed then clone.AsListProperty.SealList ()
                clone
            :> IListProperty<'p>
    interface IListProperty with
#if !FABLE_COMPILER
        member __.ElementType = typeof<'p>
#endif
        member __.ElementSpawner (o, k) = spec.Spawner (o, k) :> IProperty
        member __.SealList () =
            if not listSealed then
                listSealed <- true
        member __.ListSealed = listSealed
        member this.Has i =
            i >= 0 && i < this.Value.Length
        member this.MoveTo fromIndex toIndex =
            this.CheckIndex fromIndex
            this.CheckIndex toIndex
            if fromIndex <> toIndex then
                let prop = this.Remove (fromIndex, triggerEvent = false)
                this.Insert prop.Key prop (Some toIndex) |> ignore
                this.Value
                |> List.iteri (fun index prop ->
                    if index = toIndex then
                        triggerMoved (prop, fromIndex, toIndex)
                    elif fromIndex < toIndex then
                        if index >= fromIndex && index < toIndex then
                            triggerMoved (prop, index + 1, index)
                    else
                        if index > toIndex && index <= fromIndex then
                            triggerMoved (prop, index - 1, index)
                )
        member this.MoveBy i offset =
            (this :> IListProperty).MoveTo i (i + offset)
        member this.Swap indexA indexB =
            this.CheckIndex indexA
            this.CheckIndex indexB
            if indexA <> indexB then
                let propA = List.item indexA this.Value
                let propB = List.item indexB this.Value
                let newValue =
                    this.Value
                    |> List.mapi (fun index prop ->
                        if index = indexA then
                            propB
                        elif index = indexB then
                            propA
                        else
                            prop
                    )
                if not (this.SetValue newValue) then
                    failWith "Swap_Failed" <| sprintf "%s [%d/%d] %s <-> [%d/%d] %s" (this.ToString ()) indexA this.Value.Length (propA.ToString ()) indexB this.Value.Length (propB.ToString ())
                triggerMoved (propA, indexA, indexB)
                triggerMoved (propB, indexB, indexA)
        member __.OnMoved = onMoved.Publish
        member __.OnAdded0 = onAdded0.Publish
        member __.OnRemoved0 = onRemoved0.Publish
    interface IProperties with
        member this.Count = this.Value.Length
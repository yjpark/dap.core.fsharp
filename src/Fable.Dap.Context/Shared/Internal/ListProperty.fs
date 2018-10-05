[<AutoOpen>]
module Dap.Context.Internal.ListProperty

open System

open Dap.Prelude
open Dap.Context
open Dap.Context.Unsafe
open Dap.Context.Internal

let private newSubKey () = newLuid "SubKey"

type internal ListProperty<'p when 'p :> IProperty> private (owner, spec) =
    inherit Property<IPropertySpec<'p>, 'p list> (owner, spec, [])
    let mutable listSealed : bool = false
    let mutable valueAndIndexes : Map<Luid, 'p * Index> = Map.empty
    let onMoved = new Bus<PropertyMoved> (owner, sprintf "%s:OnMoved" spec.Luid)
    let onAdded = new Bus<'p * Index> (owner, sprintf "%s:OnAdded" spec.Luid)
    let onRemoved = new Bus<'p * Index> (owner, sprintf "%s:OnRemoved" spec.Luid)
    let onAdded0 = new Bus<IProperty * Index> (owner, sprintf "%s:OnAdded0" spec.Luid)
    let onRemoved0 = new Bus<IProperty * Index> (owner, sprintf "%s:OnRemoved0" spec.Luid)
    static member Create o (s : IPropertySpec<'p>) = new ListProperty<'p>(o, s)
    override __.Kind = PropertyKind.ListProperty
    override this.AsList = this :> IListProperty
    member this.AsListProperty = this :> IListProperty<'p>
    member this.AsProperties = this :> IProperties
    override __.ToJson (props : 'p list) =
        props
        |> List.map (fun p -> p.ToJson ())
        |> E.jsonList
    override __.WithJson value json =
        let mutable ok = true
        (* TODO
        value
        |> Map.toList
        |> List.iter (fun (k, prop) ->
            match tryCastJson (D.field k D.value) json with
            | Ok json ->
                let oneOk = prop.WithJson json
                if not oneOk then
                    ok <- false
            | Error err ->
                ok <- false
                owner.Log <| tplPropertyError "Properties:Decode_Field_Failed" key (prop.ToJson ()) err
        )
        if not ok then
            logError owner "Properties:WithJson" "Decode_Has_Error" (E.encode 4 json)
        *)
        Some (value, ok)
    override this.Clone0 o k = this.AsListProperty.Clone o k :> IProperty
    override this.SyncTo0 t = this.AsListProperty.SyncTo (t :?> IListProperty<'p>)
    override this.ToList<'p1 when 'p1 :> IProperty> () =
        if typeof<'p> = typeof<'p1> then
            this.AsList :?> IListProperty<'p1>
        else
            this.CastFailed<IListProperty<'p1>> ()
    override this.OnSealed () =
        listSealed <- true
        this.Value
        |> List.iter (fun prop ->
            prop.Seal ()
        )
    member private this.UpdateValue (overwrites : 'p list) =
        valueAndIndexes
        |> Map.toList
        |> List.map (fun (k, (prop, index))->
            //TODO sort index and trigger onMoved properly
            prop
        )
        |> this.SetValue
    member private this.CheckChange tip =
        if listSealed then
            failWith "Already_Sealed" <| sprintf "[%s] <%s> [%d] %s" spec.Luid typeof<'p>.FullName this.Value.Length tip
    member private this.Add (toIndex : ToIndex option) =
        let k = newSubKey ()
        let subSpec = spec.GetSubSpec k
        let prop = subSpec.Spawner owner k
        let index =
            match toIndex with
            | None ->
                if not (this.Value @ [prop]
                    |> this.SetValue) then
                    failWith "Add_Failed" <| sprintf "[%s] <%s> [%d] %s" spec.Luid typeof<'p>.FullName this.Value.Length prop.Spec.Key
                let index = this.Value.Length
                valueAndIndexes <- valueAndIndexes |> Map.add k (prop, index)
                index
            | Some toIndex ->
                valueAndIndexes <- valueAndIndexes |> Map.add k (prop, toIndex)
                if not (this.UpdateValue [prop]) then
                    valueAndIndexes <- valueAndIndexes |> Map.remove k
                    failWith "Add_Failed" <| sprintf "[%s] <%s> [%d] %s" spec.Luid typeof<'p>.FullName this.Value.Length prop.Spec.Key
                toIndex
        onAdded.Trigger (prop, index)
        onAdded0.Trigger (prop :> IProperty, index)
        prop
    member private this.Remove (i : Index) =
        let prop = this.Value |> List.item i
        let k = prop.Spec.Luid
        valueAndIndexes
        |> Map.tryFind k
        |> Option.map (fun (prop, i) ->
            valueAndIndexes <- valueAndIndexes |> Map.remove k
            onRemoved.Trigger (prop, i)
            onRemoved0.Trigger (prop :> IProperty, i)
            if not (this.UpdateValue []) then
                failWith "Remove_Failed" <| sprintf "[%s] <%s> [%d] %s" spec.Luid typeof<'p>.FullName this.Value.Length prop.Spec.Key
            prop
        )
    member private this.CheckIndex (i : Index) =
        if i < 0 || i >= this.Value.Length then
            failWith "Invalid_Index" <| sprintf "[%s] <%s> [%d] -> [%d]" spec.Luid typeof<'p>.FullName this.Value.Length i
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
        member this.Clear () =
            this.CheckChange "Clear"
            if this.Value.Length = 0 then
                []
            else
                let oldValue = this.Value
                if (this.SetValue []) then
                    valueAndIndexes <- Map.empty
                    oldValue
                    |> List.iteri (fun i prop ->
                        onRemoved.Trigger (prop, i)
                        onRemoved0.Trigger (prop :> IProperty, i)
                    )
                    oldValue
                else
                    failWith "Clear_Failed" <| sprintf "[%s] <%s> [%d]" spec.Luid typeof<'p>.FullName this.Value.Length
        member __.OnAdded = onAdded.Publish
        member __.OnRemoved = onRemoved.Publish
        member this.SyncTo other =
            //TODO
            ()
        member this.Clone o k =
            spec.ForClone k
            |> ListProperty<'p>.Create o
            |> this.SetupClone (Some this.AsListProperty.SyncTo)
            |> fun clone ->
                if listSealed then clone.AsListProperty.SealList ()
                clone
            :> IListProperty<'p>
    interface IListProperty with
#if !FABLE_COMPILER
        member __.ElementType = typeof<'p>
#endif
        member __.ElementSpawner o k = spec.Spawner o k :> IProperty
        member __.SealList () =
            if not listSealed then
                listSealed <- true
        member __.ListSealed = listSealed
        member this.Has i =
            i >= 0 && i < this.Value.Length
        member this.MoveTo i toIndex =
            this.CheckIndex toIndex
            let prop = this.AsListProperty.Get i
            valueAndIndexes <- valueAndIndexes |> Map.add prop.Spec.Luid (prop, toIndex)
            if not (this.UpdateValue [prop]) then
                failWith "Move_Failed" <| sprintf "[%s] <%s> [%d] %d -> %d" spec.Luid typeof<'p>.FullName this.Value.Length i toIndex
        member this.MoveBy i offset =
            (this :> IListProperty).MoveTo i (i + offset)
        member this.Swap indexA indexB =
            let propA = this.AsListProperty.Get indexA
            let propB = this.AsListProperty.Get indexB
            valueAndIndexes <-
                valueAndIndexes
                |> Map.add propA.Spec.Luid (propA, indexB)
                |> Map.add propB.Spec.Luid (propB, indexA)
            if not (this.UpdateValue [propA ; propB]) then
                failWith "Swap_Failed" <| sprintf "[%s] <%s> [%d] %d <-> %d" spec.Luid typeof<'p>.FullName this.Value.Length indexA indexB
        member __.OnMoved = onMoved.Publish
        member __.OnAdded0 = onAdded0.Publish
        member __.OnRemoved0 = onRemoved0.Publish
    interface IProperties with
        member this.Count = this.Value.Length
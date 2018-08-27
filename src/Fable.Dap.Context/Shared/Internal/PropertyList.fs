[<AutoOpen>]
module Dap.Context.Internal.PropertyList

open System
#if FABLE_COMPILER
open Fable.Core
#endif

open Dap.Prelude
open Dap.Context
open Dap.Context.Internal

let private newSubKey () = newLuid "SubKey"

type internal PropertyList<'v> (owner', spec') =
    let owner : IOwner = owner'
    let spec : PropertySpec<'v> = spec'
    let mutable sealed' : bool = false
    let mutable ver = 0
    let mutable value' : Map<Luid, IProperty<'v> * Index> = Map.empty
    let mutable value : IProperty<'v> list = []
    let onMoved = new Bus<PropertyMoved> (owner)
    let onAdded = new Bus<IProperty<'v> * Index> (owner)
    let onRemoved = new Bus<IProperty<'v> * Index> (owner)
    let onChanged = new Bus<PropertyChanged<'v>> (owner)
    let onAdded0 = new Bus<IProperty * Index> (owner)
    let onRemoved0 = new Bus<IProperty * Index> (owner)
    let onChanged0 = new Bus<PropertyChanged> (owner)
    let toJson (props : IProperty<'v> list) =
        E.nil
        (*
        props
        |> Map.toList
        |> List.map (fun (k, prop) ->
            k, prop.ToJson ()
        )|> E.object
        *)
    let updateValueAndIndexes (overwrites : IProperty<'v> list) =
        //TODO
        ()
    let checkChange tip =
        if sealed' then
            failWith "Already_Sealed" <| sprintf "[%s] <%s> [%d] %s" spec.Luid typeof<'v>.FullName value.Length tip
    let add (v : 'v) (toIndex : ToIndex option) =
        let k = newSubKey ()
        let subSpec = spec.GetSubSpec k
        let prop = new Property<'v> (owner, subSpec)
        let prop = prop.AsProperty
        prop.OnChanged.AddWatcher owner spec.Luid onChanged.Trigger
        prop.OnChanged0.AddWatcher owner spec.Luid onChanged0.Trigger
        match toIndex with
        | None ->
            let index = value'.Count
            value' <- value' |> Map.add k (prop, index)
            onAdded.Trigger (prop, index)
            onAdded0.Trigger (prop :> IProperty, index)
            value <- value @ [prop]
        | Some toIndex ->
            value' <- value' |> Map.add k (prop, toIndex)
            updateValueAndIndexes [prop]
            onAdded.Trigger (prop, toIndex)
            onAdded0.Trigger (prop :> IProperty, toIndex)
        prop
    let remove (i : Index) =
        let prop = value |> List.item i
        let k = prop.Spec.Luid
        value'
        |> Map.tryFind k
        |> Option.map (fun (prop, i) ->
            value' <- value' |> Map.remove k
            onRemoved.Trigger (prop, i)
            onRemoved0.Trigger (prop :> IProperty, i)
            updateValueAndIndexes []
            prop
        )
    let checkIndex (i : Index) =
        if i < 0 || i >= value.Length then
            failWith "Invalid_Index" <| sprintf "[%s] <%s> [%d] -> [%d]" spec.Luid typeof<'v>.FullName value.Length i
    static member Create o s = new PropertyList<'v>(o, s)
    member this.AsPropertyList = this :> IPropertyList<'v>
    member this.AsProperty = this :> IProperty
    interface IPropertyList<'v> with
        member _this.Value = value
        member _this.Spec = spec.AsSpec
        member _this.TryGet i =
            if i >= 0 && i < value.Length then
                Some <| List.item i value
            else
                None
        member this.Get i =
            checkIndex i
            List.item i value
        member this.Set i v =
            this.AsPropertyList.TryGet i
            |> function
                | Some prop ->
                    prop.SetValue v
                | None -> failWith "Not_Found" i
        member this.Add v =
            checkChange "Add"
            add v None
        member this.Insert v i =
            checkChange <| sprintf "Insert: %d" i
            if i = value.Length then
                this.AsPropertyList.Add v
            else
                checkIndex i
                add v <| Some i
        member this.Remove i =
            checkChange <| sprintf "Remove: %d" i
            checkIndex i
            remove i
        member _this.Clear () =
            checkChange "Clear"
            let oldValue = value
            value' <- Map.empty
            value <- []
            oldValue
            |> List.iteri (fun i prop ->
                onRemoved.Trigger (prop, i)
                onRemoved0.Trigger (prop :> IProperty, i)
            )
            oldValue
        member _this.OnAdded = onAdded.Publish
        member _this.OnRemoved = onRemoved.Publish
        member _this.OnChanged = onChanged.Publish
    interface IPropertyList with
        member _this.Count = value.Length
        member _this.Has i =
            i >= 0 && i < value.Length
        member this.MoveTo i toIndex =
            checkIndex toIndex
            let prop = this.AsPropertyList.Get i
            value' <- value' |> Map.add prop.Spec.Luid (prop, toIndex)
            updateValueAndIndexes [prop]
        member this.MoveBy i offset =
            (this :> IPropertyList).MoveTo i (i + offset)
        member this.Swap indexA indexB =
            let propA = this.AsPropertyList.Get indexA
            let propB = this.AsPropertyList.Get indexB
            value' <-
                value'
                |> Map.add propA.Spec.Luid (propA, indexB)
                |> Map.add propB.Spec.Luid (propB, indexA)
            updateValueAndIndexes [propA ; propB]
        member _this.OnMoved = onMoved.Publish
        member _this.OnAdded0 = onAdded0.Publish
        member _this.OnRemoved0 = onRemoved0.Publish
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
        member _this.WithJson json =
            if sealed' then
                owner.Log <| tplPropertyError "Property:Already_Sealed" spec.Luid value (E.encode 4 json)
                false
            else
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
                ok
        member this.OnChanged0 = onChanged0.Publish
    interface IJson with
        member _this.ToJson () = toJson value

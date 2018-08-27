[<AutoOpen>]
module Dap.Context.Internal.ListProperty

open System
#if FABLE_COMPILER
open Fable.Core
#endif

open Dap.Prelude
open Dap.Context
open Dap.Context.Unsafe
open Dap.Context.Internal

let private newSubKey () = newLuid "SubKey"

type internal ListProperty<'p when 'p :> IProperty> private (owner', spec') =
    let owner : IOwner = owner'
    let spec : IPropertySpec<'p> = spec'
    let mutable sealed' : bool = false
    let mutable ver = 0
    let mutable value' : Map<Luid, 'p * Index> = Map.empty
    let mutable value : 'p list = []
    let onMoved = new Bus<PropertyMoved> (owner)
    let onAdded = new Bus<'p * Index> (owner)
    let onRemoved = new Bus<'p * Index> (owner)
    let onAdded0 = new Bus<IProperty * Index> (owner)
    let onRemoved0 = new Bus<IProperty * Index> (owner)
    let onChanged = new Bus<PropertyChanged> (owner)
    let toJson (props : 'p list) =
        E.nil
        (*
        props
        |> Map.toList
        |> List.map (fun (k, prop) ->
            k, prop.ToJson ()
        )|> E.object
        *)
    let updateValueAndIndexes (overwrites : 'p list) =
        //TODO
        ()
    let checkChange tip =
        if sealed' then
            failWith "Already_Sealed" <| sprintf "[%s] <%s> [%d] %s" spec.Luid typeof<'p>.FullName value.Length tip
    let add (toIndex : ToIndex option) =
        let k = newSubKey ()
        let subSpec = spec.GetSubSpec k
        let prop = subSpec.Spawner owner k
        let prop' = prop :> IProperty
        prop'.OnChanged.AddWatcher owner spec.Luid onChanged.Trigger
        match toIndex with
        | None ->
            let index = value'.Count
            value' <- value' |> Map.add k (prop, index)
            onAdded.Trigger (prop, index)
            onAdded0.Trigger (prop', index)
            value <- value @ [prop]
        | Some toIndex ->
            value' <- value' |> Map.add k (prop, toIndex)
            updateValueAndIndexes [prop]
            onAdded.Trigger (prop, toIndex)
            onAdded0.Trigger (prop', toIndex)
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
            failWith "Invalid_Index" <| sprintf "[%s] <%s> [%d] -> [%d]" spec.Luid typeof<'p>.FullName value.Length i
    static member Create o s = new ListProperty<'p>(o, s)
    member this.AsListProperty = this :> IListProperty<'p>
    member this.AsListProperty0 = this :> IListProperty
    member this.AsProperties = this :> IProperties
    member this.AsProperty = this :> IProperty
    interface IListProperty<'p> with
        member __.Value = value
        member __.Spec = spec
        member __.TryGet i =
            if i >= 0 && i < value.Length then
                Some <| List.item i value
            else
                None
        member this.Get i =
            checkIndex i
            List.item i value
        member this.Add () =
            checkChange "Add"
            add None
        member this.Insert i =
            checkChange <| sprintf "Insert: %d" i
            if i = value.Length then
                this.AsListProperty.Add ()
            else
                checkIndex i
                add <| Some i
        member this.Remove i =
            checkChange <| sprintf "Remove: %d" i
            checkIndex i
            remove i
        member __.Clear () =
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
        member __.OnAdded = onAdded.Publish
        member __.OnRemoved = onRemoved.Publish
        member this.Clone o k =
            let clone = ListProperty<'p>.Create o <| spec.ForClone k
            this.AsProperty.ToJson () |> clone.AsProperty.WithJson |> ignore
            if sealed' then clone.AsProperty.Seal ()
            clone.AsListProperty
    interface IListProperty with
        member __.ElementType = typeof<'p>
        member __.Count = value.Length
        member __.Has i =
            i >= 0 && i < value.Length
        member this.MoveTo i toIndex =
            checkIndex toIndex
            let prop = this.AsListProperty.Get i
            value' <- value' |> Map.add prop.Spec.Luid (prop, toIndex)
            updateValueAndIndexes [prop]
        member this.MoveBy i offset =
            (this :> IListProperty).MoveTo i (i + offset)
        member this.Swap indexA indexB =
            let propA = this.AsListProperty.Get indexA
            let propB = this.AsListProperty.Get indexB
            value' <-
                value'
                |> Map.add propA.Spec.Luid (propA, indexB)
                |> Map.add propB.Spec.Luid (propB, indexA)
            updateValueAndIndexes [propA ; propB]
        member __.OnMoved = onMoved.Publish
        member __.OnAdded0 = onAdded0.Publish
        member __.OnRemoved0 = onRemoved0.Publish
    interface IProperties with
        member this.Clone1 o k = this.AsListProperty.Clone o k :> IProperties
    interface IProperty with
        member __.Kind = PropertyKind.ListProperty
        member __.Ver = ver
        member __.Spec = spec :> IPropertySpec
        member __.Seal () =
            if not sealed' then
                sealed' <- true
                value
                |> List.iter (fun prop ->
                    prop.Seal ()
                )
        member __.Sealed = sealed'
        member __.WithJson json =
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
        member this.OnChanged = onChanged.Publish
        member this.Clone0 o k = this.AsListProperty.Clone o k :> IProperty
    interface IUnsafeProperty with
        member this.AsVar = failWith (this.GetType().FullName) "Cast_Failed"
        member this.AsMap = failWith (this.GetType().FullName) "Cast_Failed"
        member this.AsList = this.AsListProperty0
        member this.AsCombo = failWith (this.GetType().FullName) "Cast_Failed"
        member this.AsCustom = failWith (this.GetType().FullName) "Cast_Failed"
        member this.ToVar<'v1> () : IVarProperty<'v1> = failWith (this.GetType().FullName) "Cast_Failed"
        member this.ToMap<'p1 when 'p1 :> IProperty> () : IMapProperty<'p1> = failWith (this.GetType().FullName) "Cast_Failed"
#if FABLE_COMPILER
        [<PassGenericsAttribute>]
#endif
        member this.ToList<'p1 when 'p1 :> IProperty> () =
            if typeof<'p> = typeof<'p1> then
                this.AsListProperty0 :?> IListProperty<'p1>
            else
                failWith (this.GetType().FullName) <| "Cast_Failed: " + typeof<'p1>.FullName
        member this.ToCustom<'p1 when 'p1 :> ICustomProperty> () : ICustomProperty<'p1> = failWith (this.GetType().FullName) "Cast_Failed"
    interface IJson with
        member __.ToJson () = toJson value

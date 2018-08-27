[<AutoOpen>]
module Dap.Context.Internal.MapProperty

open System
#if FABLE_COMPILER
open Fable.Core
#endif

open Dap.Prelude
open Dap.Context
open Dap.Context.Unsafe
open Dap.Context.Internal

type internal MapProperty<'v> (owner', spec') =
    let owner : IOwner = owner'
    let spec : PropertySpec<'v> = spec'
    let mutable sealed' : bool = false
    let mutable ver = 0
    let mutable value : Map<Key, IVarProperty<'v>> = Map.empty
    let onAdded = new Bus<IVarProperty<'v>> (owner)
    let onRemoved = new Bus<IVarProperty<'v>> (owner)
    let onChanged = new Bus<PropertyChanged<'v>> (owner)
    let onAdded0 = new Bus<IProperty> (owner)
    let onRemoved0 = new Bus<IProperty> (owner)
    let onChanged0 = new Bus<PropertyChanged> (owner)
    let toJson (props : Map<string, IVarProperty<'v>>) =
        props
        |> Map.toList
        |> List.map (fun (k, prop) ->
            k, prop.ToJson ()
        )|> E.object
    let checkChange tip =
        if sealed' then
            failWith "Already_Sealed" <| sprintf "[%s] <%s> [%d] %s" spec.Luid typeof<'v>.FullName value.Count tip
    let add (k : Key) (v : 'v) =
        let subSpec = spec.GetSubSpec k
        let prop = VarProperty<'v>.Create owner subSpec
        let prop = prop.AsVarProperty
        prop.OnChanged.AddWatcher owner spec.Luid onChanged.Trigger
        prop.OnChanged0.AddWatcher owner spec.Luid onChanged0.Trigger
        ver <- ver + 1
        value <-
            value
            |> Map.add k prop
        onAdded.Trigger prop
        onAdded0.Trigger (prop :> IProperty)
        prop
    let remove (k : Key) =
        value
        |> Map.tryFind k
        |> Option.map (fun prop ->
            ver <- ver + 1
            value <-
                value
                |> Map.remove k
            onRemoved.Trigger prop
            onRemoved0.Trigger (prop :> IProperty)
            prop
        )
    static member Create o s = new MapProperty<'v>(o, s)
    member this.AsMapProperty = this :> IMapProperty<'v>
    member this.AsMapProperty0 = this :> IMapProperty
    member this.AsProperties = this :> IProperties
    member this.AsProperty = this :> IProperty
    interface IMapProperty<'v> with
        member _this.Value = value
        member _this.Spec = spec.AsSpec
        member _this.TryGet k =
            value
            |> Map.tryFind k
        member this.Get k =
            this.AsMapProperty.TryGet k
            |> function
                | Some prop -> prop
                | None -> failWith "Not_Found" k
        member this.Set k v =
            this.AsMapProperty.TryGet k
            |> function
                | Some prop ->
                    prop.SetValue v
                | None ->
                    checkChange <| sprintf "Set: %s" k
                    let prop = add k v
                    true
        member this.Add k v =
            checkChange <| sprintf "Add: %s" k
            this.AsMapProperty.TryGet k
            |> Option.iter (fun prop ->
                failWith "Already_Exist" <| sprintf "[%s] <%s> [%s] %A -> %A" spec.Luid typeof<'v>.FullName k prop v
            )
            add k v
        member this.Remove k =
            checkChange <| sprintf "Remove: %s" k
            remove k
        member _this.Clear () =
            checkChange "Clear"
            if value = Map.empty then
                Map.empty
            else
                let oldValue = value
                ver <- ver + 1
                value <- Map.empty
                oldValue
                |> Map.iter (fun _k prop ->
                    onRemoved.Trigger prop
                    onRemoved0.Trigger (prop :> IProperty)
                )
                oldValue
        member _this.OnAdded = onAdded.Publish
        member _this.OnRemoved = onRemoved.Publish
        member _this.OnChanged = onChanged.Publish
        member this.Clone o k =
            let clone = MapProperty<'v>.Create o <| spec.ForClone k
            this.AsProperty.ToJson () |> clone.AsProperty.WithJson |> ignore
            if sealed' then clone.AsProperty.Seal ()
            clone.AsMapProperty
    interface IMapProperty with
        member _this.ElementType = typeof<'v>
        member _this.Count = value.Count
        member this.Has k =
            (this.AsMapProperty.TryGet k).IsSome
        member _this.OnAdded0 = onAdded0.Publish
        member _this.OnRemoved0 = onRemoved0.Publish
    interface IProperties with
        member this.Clone1 o k = this.AsMapProperty.Clone o k :> IProperties
    interface IProperty with
        member _this.Kind = PropertyKind.MapProperty
        member _this.Ver = ver
        member _this.Spec = spec.AsSpec1
        member _this.Seal () =
            if not sealed' then
                sealed' <- true
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
        member this.Clone0 o k = this.AsMapProperty.Clone o k :> IProperty
    interface IUnsafeProperty with
        member this.AsVar = failWith (this.GetType().FullName) "Cast_Failed"
        member this.AsMap = this.AsMapProperty0
        member this.AsList = failWith (this.GetType().FullName) "Cast_Failed"
        member this.AsCombo = failWith (this.GetType().FullName) "Cast_Failed"
        member this.ToVar<'v1> () : IVarProperty<'v1> = failWith (this.GetType().FullName) "Cast_Failed"
#if FABLE_COMPILER
        [<PassGenericsAttribute>]
#endif
        member this.ToMap<'v1> () =
            if typeof<'v> = typeof<'v1> then
                this.AsMapProperty0 :?> IMapProperty<'v1>
            else
                failWith (this.GetType().FullName) <| "Cast_Failed: " + typeof<'v1>.FullName
        member this.ToList<'v1> () : IListProperty<'v1> = failWith (this.GetType().FullName) "Cast_Failed"
    interface IJson with
        member _this.ToJson () = toJson value

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

type internal MapProperty<'p when 'p :> IProperty> private (owner', spec') =
    let owner : IOwner = owner'
    let spec : IPropertySpec<'p> = spec'
    let mutable sealed' : bool = false
    let mutable ver = 0
    let mutable value : Map<Key, 'p> = Map.empty
    let onAdded = new Bus<'p> (owner)
    let onRemoved = new Bus<'p> (owner)
    let onValueChanged = new Bus<VarPropertyChanged<'p>> (owner)
    let onAdded0 = new Bus<IProperty> (owner)
    let onRemoved0 = new Bus<IProperty> (owner)
    let onChanged = new Bus<PropertyChanged> (owner)
    let toJson (props : Map<string, 'p>) =
        props
        |> Map.toList
        |> List.map (fun (k, prop) ->
            k, prop.ToJson ()
        )|> E.object
    let checkChange tip =
        if sealed' then
            failWith "Already_Sealed" <| sprintf "[%s] <%s> [%d] %s" spec.Luid typeof<'p>.FullName value.Count tip
    let add (k : Key) =
        let subSpec = spec.GetSubSpec k
        let prop = subSpec.Spawner owner k
        let prop' = prop :> IProperty
        prop'.OnChanged.AddWatcher owner spec.Luid onChanged.Trigger
        ver <- ver + 1
        value <-
            value
            |> Map.add k prop
        onAdded.Trigger prop
        onAdded0.Trigger prop'
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
    static member Create o s = new MapProperty<'p>(o, s)
    member this.AsMapProperty = this :> IMapProperty<'p>
    member this.AsMapProperty0 = this :> IMapProperty
    member this.AsProperties = this :> IProperties
    member this.AsProperty = this :> IProperty
    interface IMapProperty<'p> with
        member __.Value = value
        member __.Spec = spec
        member __.TryGet k =
            value
            |> Map.tryFind k
        member this.Get k =
            this.AsMapProperty.TryGet k
            |> function
                | Some prop -> prop
                | None -> failWith "Not_Found" k
        member this.Add k =
            checkChange <| sprintf "Add: %s" k
            this.AsMapProperty.TryGet k
            |> Option.iter (fun prop ->
                failWith "Already_Exist" <| sprintf "[%s] <%s> [%s] -> %A" spec.Luid typeof<'p>.FullName k prop
            )
            add k
        member this.Remove k =
            checkChange <| sprintf "Remove: %s" k
            remove k
        member __.Clear () =
            checkChange "Clear"
            if value.Count = 0 then
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
        member __.OnAdded = onAdded.Publish
        member __.OnRemoved = onRemoved.Publish
        member this.Clone o k =
            let clone = MapProperty<'p>.Create o <| spec.ForClone k
            this.AsProperty.ToJson () |> clone.AsProperty.WithJson |> ignore
            if sealed' then clone.AsProperty.Seal ()
            clone.AsMapProperty
    interface IMapProperty with
        member __.ElementType = typeof<'p>
        member __.Count = value.Count
        member this.Has k =
            (this.AsMapProperty.TryGet k).IsSome
        member __.OnAdded0 = onAdded0.Publish
        member __.OnRemoved0 = onRemoved0.Publish
    interface IProperties with
        member this.Clone1 o k = this.AsMapProperty.Clone o k :> IProperties
    interface IProperty with
        member __.Kind = PropertyKind.MapProperty
        member __.Ver = ver
        member __.Spec = spec :> IPropertySpec
        member __.Seal () =
            if not sealed' then
                sealed' <- true
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
        member this.Clone0 o k = this.AsMapProperty.Clone o k :> IProperty
    interface IUnsafeProperty with
        member this.AsVar = failWith (this.GetType().FullName) "Cast_Failed"
        member this.AsMap = this.AsMapProperty0
        member this.AsList = failWith (this.GetType().FullName) "Cast_Failed"
        member this.AsCombo = failWith (this.GetType().FullName) "Cast_Failed"
        member this.AsCustom = failWith (this.GetType().FullName) "Cast_Failed"
        member this.ToVar<'v1> () : IVarProperty<'v1> = failWith (this.GetType().FullName) "Cast_Failed"
#if FABLE_COMPILER
        [<PassGenericsAttribute>]
#endif
        member this.ToMap<'p1 when 'p1 :> IProperty> () =
            if typeof<'p> = typeof<'p1> then
                this.AsMapProperty0 :?> IMapProperty<'p1>
            else
                failWith (this.GetType().FullName) <| "Cast_Failed: " + typeof<'p1>.FullName
        member this.ToList<'p1 when 'p1 :> IProperty> () : IListProperty<'p1> = failWith (this.GetType().FullName) "Cast_Failed"
        member this.ToCustom<'p1 when 'p1 :> ICustomProperty> () : ICustomProperty<'p1> = failWith (this.GetType().FullName) "Cast_Failed"
    interface IJson with
        member __.ToJson () = toJson value

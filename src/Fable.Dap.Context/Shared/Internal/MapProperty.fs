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
open Dap.Context.Internal.Property

type internal MapProperty<'p when 'p :> IProperty> private (owner, spec) =
    inherit Property<IPropertySpec<'p>, Map<Key, 'p>> (owner, spec, Map.empty)
    let mutable mapSealed : bool = false
    let onAdded = new Bus<'p> (owner, sprintf "%s:OnAdded" spec.Luid)
    let onRemoved = new Bus<'p> (owner, sprintf "%s:OnRemoved" spec.Luid)
    let onAdded0 = new Bus<IProperty> (owner, sprintf "%s:OnAdded0" spec.Luid)
    let onRemoved0 = new Bus<IProperty> (owner, sprintf "%s:OnRemoved0" spec.Luid)
    static member Create o (s : IPropertySpec<'p>) = new MapProperty<'p>(o, s)
    override __.Kind = PropertyKind.MapProperty
    override this.AsMap = this :> IMapProperty
    member this.AsMapProperty = this :> IMapProperty<'p>
    member this.AsProperties = this :> IProperties
    override this.ToJson (props : Map<string, 'p>) =
        props
        |> Map.toList
        |> List.map (fun (k, prop) ->
            k, prop.ToJson ()
        )|> E.object
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
    override this.Clone0 o k = this.AsMapProperty.Clone o k :> IProperty
#if FABLE_COMPILER
    [<PassGenericsAttribute>]
#endif
    override this.ToMap<'p1 when 'p1 :> IProperty> () =
        if typeof<'p> = typeof<'p1> then
            this.AsMap :?> IMapProperty<'p1>
        else
            this.CastFailed<IMapProperty<'p1>> ()
    override this.OnSealed () =
        mapSealed <- true
        this.Value
        |> Map.iter (fun _key prop ->
            prop.Seal ()
        )
    member private this.CheckChange tip =
        if mapSealed then
            failWith "Already_Sealed" <| sprintf "[%s] <%s> [%d] %s" spec.Luid typeof<'p>.FullName this.Value.Count tip
    member private this.Add (k : Key) =
        let subSpec = spec.GetSubSpec k
        let prop = subSpec.Spawner owner k
        if (this.Value
            |> Map.add k prop
            |> this.SetValue) then
            let prop' = prop :> IProperty
            onAdded.Trigger prop
            onAdded0.Trigger prop'
            prop
        else
            failWith "Add_Failed" <| sprintf "[%s] <%s> [%d] %s" spec.Luid typeof<'p>.FullName this.Value.Count prop.Spec.Key
    member private this.Remove (k : Key) =
        this.Value
        |> Map.tryFind k
        |> Option.map (fun prop ->
            if (this.Value
                |> Map.remove k
                |> this.SetValue) then
                onRemoved.Trigger prop
                onRemoved0.Trigger (prop :> IProperty)
                prop
            else
                failWith "Remove_Failed" <| sprintf "[%s] <%s> [%d] %s" spec.Luid typeof<'p>.FullName this.Value.Count prop.Spec.Key
        )
    interface IMapProperty<'p> with
        member this.Value = this.Value
        member __.Spec = spec
        member this.TryGet k =
            this.Value
            |> Map.tryFind k
        member this.Get k =
            this.AsMapProperty.TryGet k
            |> function
                | Some prop -> prop
                | None -> failWith "Not_Found" k
        member this.Add k =
            this.CheckChange <| sprintf "Add: %s" k
            this.AsMapProperty.TryGet k
            |> Option.iter (fun prop ->
                failWith "Already_Exist" <| sprintf "[%s] <%s> [%s] -> %A" spec.Luid typeof<'p>.FullName k prop
            )
            this.Add k
        member this.Remove k =
            this.CheckChange <| sprintf "Remove: %s" k
            this.Remove k
        member this.Clear () =
            this.CheckChange "Clear"
            if this.Value.Count = 0 then
                Map.empty
            else
                let oldValue = this.Value
                if (this.SetValue Map.empty) then
                    oldValue
                    |> Map.iter (fun _k prop ->
                        onRemoved.Trigger prop
                        onRemoved0.Trigger (prop :> IProperty)
                    )
                    oldValue
                else
                    failWith "Clear_Failed" <| sprintf "[%s] <%s> [%d]" spec.Luid typeof<'p>.FullName oldValue.Count
        member __.OnAdded = onAdded.Publish
        member __.OnRemoved = onRemoved.Publish
        member this.Clone o k =
            spec.ForClone k
            |> MapProperty<'p>.Create o
            |> this.SetupClone None
            |> fun clone ->
                if mapSealed then clone.AsMapProperty.SealMap ()
                clone
            :> IMapProperty<'p>
    interface IMapProperty with
        member __.ElementType = typeof<'p>
        member __.ElementSpawner o k = spec.Spawner o k :> IProperty
        member __.SealMap () =
            if not mapSealed then
                mapSealed <- true
        member __.MapSealed = mapSealed
        member this.Has k =
            (this.AsMapProperty.TryGet k).IsSome
        member __.OnAdded0 = onAdded0.Publish
        member __.OnRemoved0 = onRemoved0.Publish
    interface IProperties with
        member this.Count = this.Value.Count

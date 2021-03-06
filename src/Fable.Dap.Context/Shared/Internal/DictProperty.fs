[<AutoOpen>]
module Dap.Context.Internal.DictProperty

open System

open Dap.Prelude
open Dap.Context
open Dap.Context.Unsafe
open Dap.Context.Internal
open Dap.Context.Internal.Property

type internal DictProperty<'p when 'p :> IProperty> private (owner, spec) =
    inherit Property<IPropertySpec<'p>, Map<Key, 'p>> (owner, spec, Map.empty)
    let mutable dictSealed : bool = false
    let onAdded = new Bus<'p> (owner, sprintf "%s:OnAdded" spec.Luid)
    let onRemoved = new Bus<'p> (owner, sprintf "%s:OnRemoved" spec.Luid)
    let onAdded0 = new Bus<IProperty> (owner, sprintf "%s:OnAdded0" spec.Luid)
    let onRemoved0 = new Bus<IProperty> (owner, sprintf "%s:OnRemoved0" spec.Luid)
    let triggerAdded (prop : 'p) =
        onAdded.Trigger prop
        onAdded0.Trigger (prop :> IProperty)
    let triggerRemoved (prop : 'p) =
        onRemoved.Trigger prop
        onRemoved0.Trigger (prop :> IProperty)
    static member Create (o, s : IPropertySpec<'p>) = new DictProperty<'p>(o, s)
    override __.Kind = PropertyKind.DictProperty
#if FABLE_COMPILER
    member this.AsMap = this :> IDictProperty
#else
    override this.AsMap = this :> IDictProperty
#endif
    member this.AsDictProperty = this :> IDictProperty<'p>
    member this.AsProperties = this :> IProperties
    override this.ToJson (props : Map<string, 'p>) =
        props
        |> Map.toList
        |> List.map (fun (k, prop) ->
            k, prop.ToJson ()
        )|> E.object
    override this.DoLoadJson value json =
        if json.IsObject then
            let mutable failedFields : (Key * string) list = []
            let mutable failedProps : 'p list = []
            let oldValue = this.Value
            let newValue : Map<Key, 'p> =
                json.ToObjectKeys ()
                |> Seq.choose (fun k ->
                    match tryCastJson (D.field k D.json) json with
                    | Ok propJson ->
                        let prop =
                            Map.tryFind k oldValue
                            |> Option.defaultWith (fun () ->
                                let subSpec = spec.GetSubSpec k
                                subSpec.Spawner (owner, k)
                            )
                        if not (prop.LoadJson' propJson) then
                            failedProps <- prop :: failedProps
                        Some (k, prop)
                    | Error err ->
                        failedFields <- (k, err) :: failedFields
                        logAspectError this "DoLoadJson:Decode_Field_Failed" (k, err)
                        None
                )|> Map.ofSeq
            let ok = failedFields.Length = 0 && failedProps.Length = 0
            if not ok then
                logAspectError this "DoLoadJson:Decode_Got_Error" (E.encode 4 json)
                if failedFields.Length > 0 then
                    logAspectError this (sprintf "DoLoadJson:Failed_Fields: [%d]" failedFields.Length) failedProps
                if failedProps.Length > 0 then
                    logAspectError this (sprintf "DoLoadJson:Failed_Properties: [%d]" failedProps.Length)
                        (failedProps |> List.map (fun p -> (p.Spec0, p)))
            if this.SetValue newValue then
                oldValue
                |> Map.toList
                |> List.filter (fun (k, _prop) -> not (Map.containsKey k newValue))
                |> List.iter (fun (_k, prop) -> triggerRemoved prop)
                newValue
                |> Map.toList
                |> List.filter (fun (k, _prop) -> not (Map.containsKey k oldValue))
                |> List.iter (fun (_k, prop) -> triggerAdded prop)
                (ok, None)
            else
                (false, None)
        else
            (false, None)
    override this.Clone0 (o, k) = this.AsDictProperty.Clone (o, k) :> IProperty
    override this.SyncTo0 t = this.AsDictProperty.SyncTo (t :?> IDictProperty<'p>)
#if !FABLE_COMPILER
    override this.ToDict<'p1 when 'p1 :> IProperty> () =
        if typeof<'p> = typeof<'p1> then
            this.AsMap :?> IDictProperty<'p1>
        else
            this.CastFailed<IDictProperty<'p1>> ()
#endif
    override this.OnSealed () =
        dictSealed <- true
        this.Value
        |> Map.iter (fun _key prop ->
            prop.Seal ()
        )
    member private this.CheckChange tip =
        if dictSealed then
            failWith "Already_Sealed" <| sprintf "[%s] <%s> [%d] %s" spec.Luid (typeNameOf<'p> ()) this.Value.Count tip
    member private this.Add (k : Key) =
        let subSpec = spec.GetSubSpec k
        let prop = subSpec.Spawner (owner, k)
        if (this.Value
            |> Map.add k prop
            |> this.SetValue) then
            triggerAdded prop
            prop
        else
            failWith "Add_Failed" <| sprintf "[%s] <%s> [%d] %s" spec.Luid (typeNameOf<'p> ()) this.Value.Count prop.Key
    member private this.Remove (k : Key) =
        this.Value
        |> Map.tryFind k
        |> Option.map (fun prop ->
            if (this.Value
                |> Map.remove k
                |> this.SetValue) then
                triggerRemoved prop
                prop
            else
                failWith "Remove_Failed" <| sprintf "[%s] <%s> [%d] %s" spec.Luid (typeNameOf<'p> ()) this.Value.Count prop.Key
        )
    interface IDictProperty<'p> with
        member this.Value = this.Value
        member __.Spec = spec
        member this.TryGet k =
            this.Value
            |> Map.tryFind k
        member this.Get k =
            this.AsDictProperty.TryGet k
            |> function
                | Some prop -> prop
                | None -> failWith ("IDictProperty:Not_Found:" + k) this
        member this.Add k =
            this.CheckChange <| sprintf "Add: %s" k
            this.AsDictProperty.TryGet k
            |> Option.iter (fun prop ->
                failWith "Already_Exist" <| sprintf "[%s] <%s> [%s] -> %A" spec.Luid (typeNameOf<'p> ()) k prop
            )
            this.Add k
        member this.Remove k =
            this.CheckChange <| sprintf "Remove: %s" k
            this.Remove k
        member this.Clear' () =
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
                    failWith "Clear_Failed" <| sprintf "[%s] <%s> [%d]" spec.Luid (typeNameOf<'p> ()) oldValue.Count
        member __.OnAdded = onAdded.Publish
        member __.OnRemoved = onRemoved.Publish
        member this.SyncTo other =
            other.Clear ()
            this.Value
            |> Map.iter (fun key prop ->
                prop.SyncTo0 <| other.GetOrAdd key
            )
            if dictSealed
                then other.SealDict ()
        member this.Clone (o, k) =
            DictProperty<'p>.Create (o, spec.ForClone k)
            |> this.SetupClone (Some this.AsDictProperty.SyncTo)
            |> fun clone ->
                if dictSealed then clone.AsDictProperty.SealDict ()
                clone
            :> IDictProperty<'p>
    interface IDictProperty with
#if !FABLE_COMPILER
        member __.ElementType = typeof<'p>
#endif
        member __.ElementSpawner (o, k) = spec.Spawner (o, k) :> IProperty
        member __.SealDict () =
            if not dictSealed then
                dictSealed <- true
        member __.DictSealed = dictSealed
        member this.Has k =
            (this.AsDictProperty.TryGet k).IsSome
        member __.OnAdded0 = onAdded0.Publish
        member __.OnRemoved0 = onRemoved0.Publish
    interface IProperties with
        member this.Count = this.Value.Count

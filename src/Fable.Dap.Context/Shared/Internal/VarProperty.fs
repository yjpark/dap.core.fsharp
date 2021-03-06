[<AutoOpen>]
module Dap.Context.Internal.VarProperty

open System

open Dap.Prelude
open Dap.Context
open Dap.Context.Internal

type internal VarPropertySpec<'v> internal (luid, key, encoder', decoder', initValue', validator') =
    inherit PropertySpec (luid, key, (encoder' initValue'))
    let encoder : JsonEncoder<'v> = encoder'
    let decoder : JsonDecoder<'v> = decoder'
    let initValue : 'v = initValue'
    let validator : Validator<'v> option = validator'
    static member Create key encoder decoder initValue validator =
        new VarPropertySpec<'v> (key, key, encoder, decoder, initValue, validator)
        :> IVarPropertySpec<'v>
    override this.ToString () =
#if FABLE_COMPILER
        sprintf "[VarPropertySpec<_>:K='%s',L='%s',V='%A']" key luid initValue
#else
        sprintf "[VarPropertySpec<%s>:K='%s',L='%s',V='%A']" typeof<'v>.FullName key luid initValue
#endif
    interface IVarPropertySpec<'v> with
        member __.Encoder = encoder
        member __.Decoder = decoder
        member __.InitValue = initValue
        member __.Validator = validator
    interface IPropertySpec<IVarProperty<'v>> with
        member this.Spawner : PropertySpawner<IVarProperty<'v>> =
            fun (owner, key) ->
                VarProperty<'v>.Create (owner, this.ForClone key)
                :> IVarProperty<'v>

and IVarPropertySpec<'v> with
    member this.GetSubSpec subKey =
        let luid = AspectSpec.CalcSubLuid this.Luid subKey
        new VarPropertySpec<'v> (luid, subKey, this.Encoder, this.Decoder, this.InitValue, this.Validator)
        :> IVarPropertySpec<'v>
    member this.AsSubSpec (parent : IAspectSpec) =
        let luid = AspectSpec.CalcSubLuid parent.Luid this.Key
        new VarPropertySpec<'v> (luid, this.Key, this.Encoder, this.Decoder, this.InitValue, this.Validator)
        :> IVarPropertySpec<'v>
    member this.ForClone key =
        new VarPropertySpec<'v> (key, key, this.Encoder, this.Decoder, this.InitValue, this.Validator)
        :> IVarPropertySpec<'v>

and internal VarProperty<'v> private (owner, spec) =
    inherit Property<IVarPropertySpec<'v>, 'v> (owner, spec, spec.InitValue)
    let onChanged : Bus<VarPropertyChanged<'v>> = new Bus<VarPropertyChanged<'v>> (owner, sprintf "%s:OnValueChanged" spec.Luid)
    static member Create (o, s) = new VarProperty<'v> (o, s)
    override __.Kind = PropertyKind.VarProperty
#if FABLE_COMPILER
    member this.AsVar = this :> IVarProperty
#else
    override this.AsVar = this :> IVarProperty
#endif
    member this.AsVarProperty = this :> IVarProperty<'v>
    member this.AsValue = this :> IValue<'v>
    override __.ToJson (v : 'v) =
        spec.Encoder v
    override this.DoLoadJson _value json =
        tryCastJson spec.Decoder json
        |> function
            | Ok v ->
                (true, Some v)
            | Error err ->
                logAspectError this "DoLoadJson:Decode_Failed" (this.Value, E.encode 4 json, err)
                (false, None)
    override this.Clone0 (o, k) = this.AsVarProperty.Clone (o, k) :> IProperty
    override this.SyncTo0 t = this.AsVarProperty.SyncTo (t :?> IVarProperty<'v>)
#if !FABLE_COMPILER
    override this.ToVar<'v1> () =
        if typeof<'v> = typeof<'v1> then
            this.AsVar :?> IVarProperty<'v1>
        else
            this.CastFailed<IVarProperty<'v1>> ()
#endif
    override this.ShouldSetValue (v : 'v) =
        this.Spec.Validator
        |> Option.map (fun validator ->
            let valid = validator.Check this.AsValue v
            if not valid then
                logAspectDebug this "ShouldSetValue:Validator_Denied" (this.Value, v, validator)
            valid
        )|> Option.defaultValue true
    override this.OnValueChanged (old : 'v) =
        let evt : VarPropertyChanged<'v> =
            {
                Spec = spec
                Old = old
                New = this.Value
            }
        onChanged.Trigger evt
    interface IVarProperty<'v> with
        member __.Spec = spec
        member this.Value = this.Value
        member this.SetValue' v = this.SetValue v
        member __.OnChanged = onChanged.Publish
        member this.SyncTo (other : IVarProperty<'v>) =
            other.SetValue' this.Value |> ignore
        member this.Clone (o, k) =
            VarProperty<'v>.Create (o, spec.ForClone k)
            |> this.SetupClone (Some this.AsVarProperty.SyncTo)
            :> IVarProperty<'v>
#if FABLE_COMPILER
    interface IVarProperty
#else
    interface IVarProperty with
        member __.ValueType = typeof<'v>
#endif

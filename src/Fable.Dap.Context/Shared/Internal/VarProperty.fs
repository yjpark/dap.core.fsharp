[<AutoOpen>]
module Dap.Context.Internal.VarProperty

open System
#if FABLE_COMPILER
open Fable.Core
#endif

open Dap.Prelude
open Dap.Context
open Dap.Context.Internal

type internal VarProperty<'v> private (owner, spec) =
    inherit Property<IVarPropertySpec<'v>, 'v> (owner, spec, spec.InitValue)
    let onValueChanged : Bus<VarPropertyChanged<'v>> = new Bus<VarPropertyChanged<'v>> (owner)
    static member Create o (s : IVarPropertySpec<'v>) = new VarProperty<'v> (o, s)
    override __.Kind = PropertyKind.VarProperty
    override this.AsVar = this :> IVarProperty
    member this.AsVarProperty = this :> IVarProperty<'v>
    override __.ToJson (v : 'v) =
        spec.Encoder v
    override this.WithJson _value json =
        tryCastJson spec.Decoder json
        |> function
            | Ok v ->
                Some (v, true)
            | Error err ->
                owner.Log <| tplPropertyError "Property:Decode_Failed" spec.Luid this.Value (E.encode 4 json, err)
                None
    override this.Clone0 o k = (this :> IVarProperty<'v>) .Clone o k :> IProperty
#if FABLE_COMPILER
    [<PassGenericsAttribute>]
#endif
    override this.ToVar<'v1> () =
        if typeof<'v> = typeof<'v1> then
            this.AsVar :?> IVarProperty<'v1>
        else
            this.CastFailed<IVarProperty<'v1>> ()
    override this.ShouldSetValue (v : 'v) =
        this.Spec.Validator
        |> Option.map (fun validator ->
            let valid = validator this.AsVarProperty v
            if not valid then
                owner.Log <| tplPropertyDebug "Property:Invalid_Value" spec.Luid this.Value v
            valid
        )|> Option.defaultValue true
    override this.OnValueChanged (old : 'v) =
        let evt : VarPropertyChanged<'v> =
            {
                Spec = spec
                Old = old
                New = this.Value
            }
        onValueChanged.Trigger evt
    interface IVarProperty<'v> with
        member __.Spec = spec
        member this.Value = this.Value
        member this.SetValue v = this.SetValue v
        member __.OnValueChanged = onValueChanged.Publish
        member this.Clone o k =
            spec.ForClone k
            |> VarProperty<'v>.Create o
            |> this.SetupClone None
            :> IVarProperty<'v>
    interface IVarProperty with
        member __.ValueType = typeof<'v>
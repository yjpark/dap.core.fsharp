[<AutoOpen>]
module Dap.Context.Internal.Spec

open System
#if FABLE_COMPILER
open Fable.Core
#endif

open Dap.Prelude
open Dap.Context

type internal AspectSpec internal (luid', key') =
    let luid : Luid = luid'
    let key : Key = key'
    interface IAspectSpec with
        member __.Luid = luid
        member __.Key = key

type IAspectSpec with
    static member CalcSubLuid (parentLuid : Luid) (subKey : Key) =
        sprintf "%s.%s" parentLuid subKey
    static member Create key =
        new AspectSpec (key, key)
        :> IAspectSpec
    member this.GetSubSpec subKey =
        let luid = AspectSpec.CalcSubLuid this.Luid subKey
        new AspectSpec (luid, subKey)
        :> IAspectSpec
    member this.AsSubSpec (parent : IAspectSpec) =
        let luid = AspectSpec.CalcSubLuid parent.Luid this.Key
        new AspectSpec (luid, this.Key)
        :> IAspectSpec

type internal PropertySpec internal (luid, key, initValue') =
    inherit AspectSpec (luid, key)
    let initValue : Json = initValue'
    interface IPropertySpec with
        member __.InitValue = initValue

type IPropertySpec with
    static member Create (key, initValue) =
        new PropertySpec (key, key, initValue)
        :> IPropertySpec
    member this.GetSubSpec subKey =
        let luid = IAspectSpec.CalcSubLuid this.Luid subKey
        new PropertySpec (luid, subKey, this.InitValue)
        :> IPropertySpec
    member this.AsSubSpec (parent : IAspectSpec) =
        let luid = IAspectSpec.CalcSubLuid parent.Luid this.Key
        new PropertySpec (luid, this.Key, this.InitValue)
        :> IPropertySpec
    member this.ForClone key =
        IPropertySpec.Create (key, this.InitValue)

type internal VarPropertySpec<'v> internal (luid, key, encoder', decoder', initValue', validator') =
    inherit PropertySpec (luid, key, (encoder' initValue'))
    let encoder : JsonEncoder<'v> = encoder'
    let decoder : JsonDecoder<'v> = decoder'
    let initValue : 'v = initValue'
    let validator : Validator<'v> option = validator'
    interface IVarPropertySpec<'v> with
        member __.Encoder = encoder
        member __.Decoder = decoder
        member __.InitValue = initValue
        member __.Validator = validator

type IVarPropertySpec<'v> with
#if FABLE_COMPILER
    [<PassGenericsAttribute>]
#endif
    static member Create (key, encoder, decoder, initValue, validator) =
        new VarPropertySpec<'v> (key, key, encoder, decoder, initValue, validator)
        :> IVarPropertySpec<'v>
    static member GetSubSpec' (this : IVarPropertySpec<'v>) subKey =
        let luid = AspectSpec.CalcSubLuid this.Luid subKey
        new VarPropertySpec<'v> (luid, subKey, this.Encoder, this.Decoder, this.InitValue, this.Validator)
        :> IVarPropertySpec<'v>
    static member AsSubSpec' (this : IVarPropertySpec<'v>) (parent : IAspectSpec) =
        let luid = AspectSpec.CalcSubLuid parent.Luid this.Key
        new VarPropertySpec<'v> (luid, this.Key, this.Encoder, this.Decoder, this.InitValue, this.Validator)
        :> IVarPropertySpec<'v>
    member this.ForClone key =
        IVarPropertySpec<'v>.Create (key, this.Encoder, this.Decoder, this.InitValue, this.Validator)

type internal PropertySpec<'p when 'p :> IProperty> internal (luid, key, initValue, spawner') =
    inherit PropertySpec (luid, key, initValue)
    let spawner : PropertySpawner<'p> = spawner'
    interface IPropertySpec<'p> with
        member __.Spawner = spawner

type IPropertySpec<'p when 'p :> IProperty> with
    static member Create (key, initValue, spawner) =
        new PropertySpec<'p> (key, key, initValue, spawner)
        :> IPropertySpec<'p>
    member this.GetSubSpec subKey =
        let luid = IAspectSpec.CalcSubLuid this.Luid subKey
        new PropertySpec<'p> (luid, subKey, this.InitValue, this.Spawner)
        :> IPropertySpec<'p>
    member this.AsSubSpec (parent : IAspectSpec) =
        let luid = IAspectSpec.CalcSubLuid parent.Luid this.Key
        new PropertySpec<'p> (luid, this.Key, this.InitValue, this.Spawner)
        :> IPropertySpec<'p>
    member this.ForClone key =
        IPropertySpec<'p>.Create (key, this.InitValue, this.Spawner)
    member this.Spawn owner =
        this.Spawner owner ((this :> IPropertySpec).Key)

type internal ContextSpec internal (luid', kind', propertiesSpawner') =
    let luid : Luid = luid'
    let kind : Kind = kind'
    let propertiesSpawner : (IOwner -> IProperties) = propertiesSpawner'
    interface IContextSpec with
        member __.Luid = luid
        member __.Kind = kind
        member __.PropertiesSpawner owner = propertiesSpawner owner

type IContextSpec with
    static member Create (kind, propertiesSpawner) =
        let luid = newLuid kind
        new ContextSpec (luid, kind, propertiesSpawner)
        :> IContextSpec

[<AutoOpen>]
module Dap.Context.Internal.Spec

open System
#if FABLE_COMPILER
open Fable.Core
#endif

open Dap.Prelude
open Dap.Context

type internal AspectSpec internal (kind', luid', key') =
    let kind : Kind = kind'
    let luid : Luid = luid'
    let key : Key = key'
    static member Create0 kind key =
        new AspectSpec (kind, key, key)
        :> IAspectSpec
    interface IAspectSpec with
        member __.Kind = kind
        member __.Luid = luid
        member __.Key = key

type IAspectSpec with
    static member CalcSubLuid (parentLuid : Luid) (subKey : Key) =
        sprintf "%s.%s" parentLuid subKey
    member this.GetSubSpec subKey =
        let luid = AspectSpec.CalcSubLuid this.Luid subKey
        new AspectSpec (this.Kind, luid, subKey)
        :> IAspectSpec
    member this.AsSubSpec (parent : IAspectSpec) =
        let luid = AspectSpec.CalcSubLuid parent.Luid this.Key
        new AspectSpec (this.Kind, luid, this.Key)
        :> IAspectSpec

type internal PropertySpec internal (kind, luid, key, initValue') =
    inherit AspectSpec (kind, luid, key)
    let initValue : Json = initValue'
    abstract ValidatorKind : Kind option with get
    default __.ValidatorKind = None
    static member Create1 kind key initValue =
        new PropertySpec (kind, key, key, initValue)
        :> IPropertySpec
    interface IPropertySpec with
        member __.InitValue = initValue
        member this.ValidatorKind = this.ValidatorKind

type IPropertySpec with
    member this.GetSubSpec subKey =
        let luid = IAspectSpec.CalcSubLuid this.Luid subKey
        new PropertySpec (this.Kind, luid, subKey, this.InitValue)
        :> IPropertySpec
    member this.AsSubSpec (parent : IAspectSpec) =
        let luid = IAspectSpec.CalcSubLuid parent.Luid this.Key
        new PropertySpec (this.Kind, luid, this.Key, this.InitValue)
        :> IPropertySpec
    member this.ForClone key =
        new PropertySpec (this.Kind, key, key, this.InitValue)
        :> IPropertySpec

type internal PropertySpec<'p when 'p :> IProperty> internal (kind, luid, key, initValue, spawner') =
    inherit PropertySpec (kind, luid, key, initValue)
    let spawner : PropertySpawner<'p> = spawner'
    static member Create2 kind key initValue spawner =
        new PropertySpec<'p> (kind, key, key, initValue, spawner)
        :> IPropertySpec<'p>
    interface IPropertySpec<'p> with
        member __.Spawner = spawner

type IPropertySpec<'p when 'p :> IProperty> with
    member this.GetSubSpec subKey =
        let luid = IAspectSpec.CalcSubLuid this.Luid subKey
        new PropertySpec<'p> (this.Kind, luid, subKey, this.InitValue, this.Spawner)
        :> IPropertySpec<'p>
    member this.AsSubSpec (parent : IAspectSpec) =
        let luid = IAspectSpec.CalcSubLuid parent.Luid this.Key
        new PropertySpec<'p> (this.Kind, luid, this.Key, this.InitValue, this.Spawner)
        :> IPropertySpec<'p>
    member this.ForClone key =
        new PropertySpec<'p> (this.Kind, key, key, this.InitValue, this.Spawner)
        :> IPropertySpec<'p>
    member this.Spawn owner =
        this.Spawner owner ((this :> IPropertySpec).Key)

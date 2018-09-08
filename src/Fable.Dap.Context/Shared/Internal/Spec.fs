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
    static member Create0 key =
        new AspectSpec (key, key)
        :> IAspectSpec
    interface IAspectSpec with
        member __.Luid = luid
        member __.Key = key

type IAspectSpec with
    static member CalcSubLuid (parentLuid : Luid) (subKey : Key) =
        sprintf "%s.%s" parentLuid subKey
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
    static member Create1 key initValue =
        new PropertySpec (key, key, initValue)
        :> IPropertySpec
    interface IPropertySpec with
        member __.InitValue = initValue

type IPropertySpec with
    member this.GetSubSpec subKey =
        let luid = IAspectSpec.CalcSubLuid this.Luid subKey
        new PropertySpec (luid, subKey, this.InitValue)
        :> IPropertySpec
    member this.AsSubSpec (parent : IAspectSpec) =
        let luid = IAspectSpec.CalcSubLuid parent.Luid this.Key
        new PropertySpec (luid, this.Key, this.InitValue)
        :> IPropertySpec
    member this.ForClone key =
        new PropertySpec (key, key, this.InitValue)
        :> IPropertySpec

type internal PropertySpec<'p when 'p :> IProperty> internal (luid, key, initValue, spawner') =
    inherit PropertySpec (luid, key, initValue)
    let spawner : PropertySpawner<'p> = spawner'
    static member Create2 key initValue spawner =
        new PropertySpec<'p> (key, key, initValue, spawner)
        :> IPropertySpec<'p>
    interface IPropertySpec<'p> with
        member __.Spawner = spawner

type IPropertySpec<'p when 'p :> IProperty> with
    member this.GetSubSpec subKey =
        let luid = IAspectSpec.CalcSubLuid this.Luid subKey
        new PropertySpec<'p> (luid, subKey, this.InitValue, this.Spawner)
        :> IPropertySpec<'p>
    member this.AsSubSpec (parent : IAspectSpec) =
        let luid = IAspectSpec.CalcSubLuid parent.Luid this.Key
        new PropertySpec<'p> (luid, this.Key, this.InitValue, this.Spawner)
        :> IPropertySpec<'p>
    member this.ForClone key =
        new PropertySpec<'p> (key, key, this.InitValue, this.Spawner)
        :> IPropertySpec<'p>
    member this.Spawn owner =
        this.Spawner owner ((this :> IPropertySpec).Key)

[<AutoOpen>]
module Dap.Context.Internal.GenericSpec

open System
#if FABLE_COMPILER
open Fable.Core
#endif

open Dap.Prelude
open Dap.Context

//Fable has issue if put this in Spec.fs
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

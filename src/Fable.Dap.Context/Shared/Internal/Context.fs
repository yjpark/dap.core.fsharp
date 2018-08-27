[<AutoOpen>]
module Dap.Context.Internal.Context

#if FABLE_COMPILER
open Fable.Core
#endif

open Dap.Prelude
open Dap.Context
open Dap.Context.Internal

type internal Context (logging', spec') =
    let spec : ContextSpec = spec'
    let owner' : Owner = new Owner (logging', spec'.Luid)
    let owner = owner'.AsOwner
    let properties = spec.PropertiesSpawner owner
    static member Create l s = new Context (l, s)
    member this.AsContext = this :> IContext
    member this.AsOwner = this :> IOwner
    interface IContext with
        member _this.Dispose () = owner'.Dispose ()
        member _this.Properties = properties
        member _this.Clone0 logging =
                Context.Create logging spec
                :> IContext
    interface IJson with
        member this.ToJson () = properties.ToJson ()
    interface IOwner with
        member _this.Log m = owner.Log m
        member _this.Luid = owner.Luid
        member _this.Disposed = owner.Disposed

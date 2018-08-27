[<AutoOpen>]
module Dap.Context.Internal.Context

#if FABLE_COMPILER
open Fable.Core
#endif

open Dap.Prelude
open Dap.Context
open Dap.Context.Internal

type internal Context (logging', spec') =
    let spec : IContextSpec = spec'
    let owner' : Owner = new Owner (logging', spec'.Luid)
    let owner = owner'.AsOwner
    let properties = spec.PropertiesSpawner owner
    static member Create l s = new Context (l, s)
    member this.AsContext = this :> IContext
    member this.AsOwner = this :> IOwner
    interface IContext with
        member __.Spec = spec
        member __.Properties = properties
        member __.Dispose () = owner'.Dispose ()
        member __.Clone0 logging =
                Context.Create logging spec
                :> IContext
    interface IJson with
        member this.ToJson () = properties.ToJson ()
    interface IOwner with
        member __.Log m = owner.Log m
        member __.Luid = owner.Luid
        member __.Disposed = owner.Disposed

[<AutoOpen>]
module Dap.Context.Internal.Context

#if FABLE_COMPILER
open Fable.Core
#endif

open Dap.Prelude
open Dap.Context
open Dap.Context.Internal
open Dap.Context.Unsafe

type IContext with
#if FABLE_COMPILER
    [<PassGenericsAttribute>]
#endif
    member this.SetupClone<'c when 'c :> IContext> (extraSetup : ('c -> unit) option) (clone : 'c) =
        extraSetup
        |> Option.iter (fun setup ->
            setup clone
        )
        let clone' = clone :> IContext
        this.Properties.ToJson () |> clone'.Properties.WithJson |> ignore
        if this.Properties.Sealed then clone'.Properties.Seal ()
        clone

[<AbstractClass>]
type BaseContext<'c, 's, 'p when 'c :> IContext and 's :> IContextSpec<'p> and 'p :> IProperties> (logging', spec' : 's) =
    let spec : 's = spec'
    let owner = new Owner (logging', spec.Luid)
    let properties : 'p = spec.SpawnProperties owner
    let channels : IChannels = new Channels (owner, AspectSpec.Create0 ChannelsKey) :> IChannels
    // abstract members
    abstract member Self : 'c with get
    abstract member Spawn : ILogging -> 'c
    // virtual members
    abstract member OnDisposed : unit -> unit
    abstract member SetupCloneBefore : 'c -> unit
    abstract member SetupCloneAfter : 'c -> unit
    default __.OnDisposed () = ()
    default __.SetupCloneBefore (_c : 'c) = ()
    default __.SetupCloneAfter (_c : 'c) = ()
    member __.Spec = spec
    member __.Owner = owner
    member __.Properties = properties
    member __.Channels = channels
    member this.AsContext = this :> IContext<'c, 's, 'p>
    member this.AsContext0 = this :> IContext
    member this.AsOwner = this :> IOwner
    interface IContext<'c, 's, 'p> with
        member this.Self = this.Self
        member __.Spec = spec
        member this.Clone l =
            this.Spawn l
            |> this.SetupClone (Some this.SetupCloneBefore)
            |> fun clone ->
                this.SetupCloneAfter clone
                clone
    interface IContext<'p> with
        member __.Properties = properties
    interface IContext with
        member this.Dispose () =
            if not owner.Disposed then
                if owner.Dispose () then
                    this.OnDisposed ()
                    true
                else false
            else false
        member __.Spec = spec :> IContextSpec
        member __.Properties = properties :> IProperties
        member __.Channels = channels
        member this.Clone0 l = this.AsContext.Clone l :> IContext
    interface IUnsafeContext with
#if FABLE_COMPILER
        [<PassGenericsAttribute>]
#endif
        member this.ToContext<'c1 when 'c1 :> IContext> () = this :> IContext :?> 'c1
    interface IJson with
        member this.ToJson () = properties.ToJson ()
    interface IOwner with
        member __.Log m = owner.Log m
        member __.Luid = owner.Luid
        member __.Disposed = owner.Disposed

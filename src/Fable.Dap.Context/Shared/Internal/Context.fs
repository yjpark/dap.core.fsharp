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
    let properties' = Properties.Create owner <| PropertySpec.Create NoLuid NoKey spec.InitValue
    let properties = properties'.AsProperties
    static member Create l s = new Context (l, s)
    member this.AsContext = this :> IContext
    member this.AsOwner = this :> IOwner
    interface IContext with
        member _this.Dispose () = owner'.Dispose ()
        member _this.Properties = properties
    interface IProperties with
        member _this.Value = properties.Value
        member _this.SealCombo () = properties.SealCombo ()
        member _this.ComboSealed = properties.ComboSealed
        member _this.TryGet k = properties.TryGet k
        member _this.Has k = properties.Has k
        member _this.Get k = properties.Get k
        member _this.Add prop = properties.Add prop
#if FABLE_COMPILER
        [<PassGenericsAttribute>]
#endif
        member _this.Add<'v> subSpec = properties.Add<'v> subSpec
#if FABLE_COMPILER
        [<PassGenericsAttribute>]
#endif
        member _this.AddMap<'v> subSpec = properties.AddMap<'v> subSpec
#if FABLE_COMPILER
        [<PassGenericsAttribute>]
#endif
        member _this.AddList<'v> subSpec = properties.AddList<'v> subSpec
        member _this.OnAdded = properties.OnAdded

    interface IProperty with
        member _this.Ver = properties.Ver
        member _this.Spec = properties.Spec
        member _this.Seal () = properties.Seal ()
        member _this.Sealed = properties.Sealed
        member _this.WithJson json = properties.WithJson json
        member _this.OnChanged0 = properties.OnChanged0
    interface IJson with
        member this.ToJson () = properties.ToJson ()
    interface IOwner with
        member _this.Log m = owner.Log m
        member _this.Luid = owner.Luid
        member _this.Disposed = owner.Disposed

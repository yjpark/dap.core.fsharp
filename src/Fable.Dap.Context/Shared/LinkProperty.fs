[<AutoOpen>]
module Dap.Context.LinkProperty

#if FABLE_COMPILER
open Fable.Core
#endif

open Dap.Prelude
open Dap.Context.Unsafe
open Dap.Context.Internal
open Dap.Context.Helper

let private spec key =
    new PropertySpec (key, key, E.nil)
    :> IPropertySpec

type LinkProperty<'t when 't :> IProperty> (target : 't, owner, key) =
    inherit CustomProperty<LinkProperty<'t>, IPropertySpec, 't> (owner, spec key, target)
    static member Create t o k = new LinkProperty<'t> (t, o, k)
    static member Default t = LinkProperty<'t>.Create t noOwner NoKey
    static member AddToCombo (t : 't) k (combo : IComboProperty) =
        combo.AddCustom<LinkProperty<'t>> (LinkProperty<'t>.Create t, k)
    override this.Self = this
    override __.Spawn o k = LinkProperty<'t>.Create target o k
    override __.SyncTo other = ()
    override __.ToJson _value = E.nil
    override __.WithJson _value _json = None

type IComboProperty with
    member this.AddLink<'t when 't :> IProperty> (target : 't, key) =
        LinkProperty<'t>.AddToCombo target key this

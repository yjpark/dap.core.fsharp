[<AutoOpen>]
module Dap.Context.Unsafe.Types

open Dap.Context

type IUnsafeProperty =
#if FABLE_COMPILER
    interface end
#else
    abstract AsVar : IVarProperty with get
    abstract AsMap : IDictProperty with get
    abstract AsList : IListProperty with get
    abstract AsCombo : IComboProperty with get
    abstract AsCustom : ICustomProperty with get
    abstract ToVar<'v1> : unit -> IVarProperty<'v1>
    abstract ToDict<'p1 when 'p1 :> IProperty> : unit -> IDictProperty<'p1>
    abstract ToList<'p1 when 'p1 :> IProperty> : unit -> IListProperty<'p1>
    abstract ToCustom<'p1 when 'p1 :> ICustomProperty> : unit -> ICustomProperty<'p1>
#endif

#if !FABLE_COMPILER
type IProperty with
    member this.AsVar = (this :?> IUnsafeProperty) .AsVar
    member this.AsMap = (this :?> IUnsafeProperty) .AsMap
    member this.AsList = (this :?> IUnsafeProperty) .AsList
    member this.AsCombo = (this :?> IUnsafeProperty) .AsCombo
    member this.AsCustom = (this :?> IUnsafeProperty) .AsCustom
    member this.ToVar<'v1> () = (this :?> IUnsafeProperty) .ToVar<'v1> ()
    member this.ToDict<'p1 when 'p1 :> IProperty> () = (this :?> IUnsafeProperty) .ToDict<'p1> ()
    member this.ToList<'p1 when 'p1 :> IProperty> () = (this :?> IUnsafeProperty) .ToList<'p1> ()
    member this.ToCustom<'p1 when 'p1 :> ICustomProperty> () = (this :?> IUnsafeProperty) .ToCustom<'p1> ()
#endif

type IUnsafeContext =
    abstract ToContext<'c1 when 'c1 :> IContext> : unit -> 'c1

type IContext with
    member this.ToContext<'c1 when 'c1 :> IContext> () = (this :?> IUnsafeContext) .ToContext<'c1> ()

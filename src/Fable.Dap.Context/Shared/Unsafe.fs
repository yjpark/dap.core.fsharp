module Dap.Context.Unsafe

#if FABLE_COMPILER
open Fable.Core
#endif

type IUnsafeProperty =
    inherit IProperty
    abstract AsVar : IVarProperty with get
    abstract AsMap : IMapProperty with get
    abstract AsList : IListProperty with get
    abstract AsCombo : IComboProperty with get
    abstract AsCustom : ICustomProperty with get
    abstract ToVar<'v1> : unit -> IVarProperty<'v1>
    abstract ToMap<'p1 when 'p1 :> IProperty> : unit -> IMapProperty<'p1>
    abstract ToList<'p1 when 'p1 :> IProperty> : unit -> IListProperty<'p1>
    abstract ToCustom<'p1 when 'p1 :> ICustomProperty> : unit -> ICustomProperty<'p1>

type IProperty with
    member this.AsVar = (this :?> IUnsafeProperty) .AsVar
    member this.AsMap = (this :?> IUnsafeProperty) .AsMap
    member this.AsList = (this :?> IUnsafeProperty) .AsList
    member this.AsCombo = (this :?> IUnsafeProperty) .AsCombo
    member this.AsCustom = (this :?> IUnsafeProperty) .AsCustom
#if FABLE_COMPILER
    [<PassGenericsAttribute>]
#endif
    member this.ToVar<'v1> () = (this :?> IUnsafeProperty) .ToVar<'v1> ()
#if FABLE_COMPILER
    [<PassGenericsAttribute>]
#endif
    member this.ToMap<'p1 when 'p1 :> IProperty> () = (this :?> IUnsafeProperty) .ToMap<'p1> ()
#if FABLE_COMPILER
    [<PassGenericsAttribute>]
#endif
    member this.ToList<'p1 when 'p1 :> IProperty> () = (this :?> IUnsafeProperty) .ToList<'p1> ()
#if FABLE_COMPILER
    [<PassGenericsAttribute>]
#endif
    member this.ToCustom<'p1 when 'p1 :> ICustomProperty> () = (this :?> IUnsafeProperty) .ToCustom<'p1> ()
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
    abstract ToVar<'v1> : unit -> IVarProperty<'v1>
    abstract ToMap<'v1> : unit -> IMapProperty<'v1>
    abstract ToList<'v1> : unit -> IListProperty<'v1>

type IProperty with
    member this.AsVar = (this :?> IUnsafeProperty) .AsVar
    member this.AsMap = (this :?> IUnsafeProperty) .AsMap
    member this.AsList = (this :?> IUnsafeProperty) .AsList
    member this.AsCombo = (this :?> IUnsafeProperty) .AsCombo
#if FABLE_COMPILER
    [<PassGenericsAttribute>]
#endif
    member this.ToVar<'v1> () = (this :?> IUnsafeProperty) .ToVar<'v1> ()
#if FABLE_COMPILER
    [<PassGenericsAttribute>]
#endif
    member this.ToMap<'v1> () = (this :?> IUnsafeProperty) .ToMap<'v1> ()
#if FABLE_COMPILER
    [<PassGenericsAttribute>]
#endif
    member this.ToList<'v1> () = (this :?> IUnsafeProperty) .ToList<'v1> ()
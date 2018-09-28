[<AutoOpen>]
module Dap.Context.Unsafe.Context

#if FABLE_COMPILER
open Fable.Core
#endif

open Dap.Context

type IContext with
#if FABLE_COMPILER
    [<PassGenericsAttribute>]
#endif
    member this.AsCombo = this :?> ComboContext
    member this.ToDict<'p when 'p :> IProperty> () = this :?> DictContext<'p>
    member this.ToList<'p when 'p :> IProperty> () = this :?> ListContext<'p>
    member this.ToCustom<'p when 'p :> ICustomProperties> () = this :?> CustomContext<'p>

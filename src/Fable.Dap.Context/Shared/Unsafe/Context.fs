[<AutoOpen>]
module Dap.Context.Unsafe.Context

open Dap.Context

type IContext with
    member this.AsCombo = this :?> ComboContext
    member this.ToDict<'p when 'p :> IProperty> () = this :?> DictContext<'p>
    member this.ToList<'p when 'p :> IProperty> () = this :?> ListContext<'p>
    member this.ToCustom<'p when 'p :> ICustomProperties> () = this :?> CustomContext<'p>

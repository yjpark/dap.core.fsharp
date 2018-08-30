[<AutoOpen>]
module Dap.Context.Builder.Value.Dsl

open Dap.Prelude
open Dap.Context

let combo = new Combo.Builder ()
let context kind = new Context.Builder (kind)

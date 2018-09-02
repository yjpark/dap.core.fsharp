[<AutoOpen>]
module Dap.Context.Builder.Dsl

open Dap.Prelude
open Dap.Context

let combo = new Combo.Builder ()
let extend target = new Combo.ExtendBuilder (target)
let context kind = new Context.Builder (kind)

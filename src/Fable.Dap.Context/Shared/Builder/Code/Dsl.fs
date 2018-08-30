[<AutoOpen>]
module Dap.Context.Builder.Code.Dsl

open Dap.Prelude
open Dap.Context

let combo kind = new Combo.Builder (kind)
let context kind = new Context.Builder (kind)

[<AutoOpen>]
module Dap.Context.Builder.Dsl

open Dap.Prelude
open Dap.Context

let combo = new Combo.Builder ()
let extend parent = new Combo.ExtendBuilder (parent)
let context kind = new Context.Builder (kind)

let list<'p when 'p :> IProperty> spawner = new List.Builder<'p> (spawner)
let list0 spawner = new List.Builder<IProperty> (spawner)

let union = Union.Builder ()
let fields = combo

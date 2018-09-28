[<AutoOpen>]
module Dap.Context.Builder.Helper

open Microsoft.FSharp.Quotations

open Dap.Prelude
open Dap.Context

let combo = new Combo.Builder ()

let extend (parent : IComboProperty) = new Combo.ExtendBuilder (parent)

[<AutoOpen>]
module Dap.Context.ContextExtension

open Dap.Prelude
open Dap.Context

type IContext with
    static member Default kind = Context.combo kind :> IContext
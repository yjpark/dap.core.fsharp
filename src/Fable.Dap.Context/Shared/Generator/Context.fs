[<RequireQualifiedAccess>]
module Dap.Context.Generator.Context

open System.Reflection

open Dap.Prelude
open Dap.Context

type Generator (template : IContext) =
    interface IGenerator with
        member __.GenerateClass kind = []
        member __.GenerateBuilder kind = []

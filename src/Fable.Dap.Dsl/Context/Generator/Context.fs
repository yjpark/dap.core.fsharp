[<RequireQualifiedAccess>]
module Dap.Context.Generator.Context

open System.Reflection

open Dap.Prelude
open Dap.Context

type ClassGenerator (template : IContext) =
    interface IGenerator<ClassParam> with
        member __.Generate param = []

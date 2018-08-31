[<AutoOpen>]
module Dap.Context.Generator.Types

open Dap.Prelude
open Dap.Context

type Lines = string list

type IGenerator =
    abstract GenerateClass : Kind -> Lines
    abstract GenerateBuilder : Kind -> Lines
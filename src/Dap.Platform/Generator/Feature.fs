[<RequireQualifiedAccess>]
module Dap.Platform.Generator.Feature

open Dap.Prelude
open Dap.Context
open Dap.Context.Meta
open Dap.Context.Generator
open Dap.Context.Generator.Util

type InterfaceGenerator (meta : ContextMeta) =
    inherit Dap.Context.Generator.Context.InterfaceGenerator (meta)
    override __.GetExtraInherits (_param : ContextParam) =
        [
            "    inherit IFeature"
        ]

type ClassGenerator (meta : ContextMeta) =
    inherit Dap.Context.Generator.Context.ClassGenerator (meta)
    override __.GetExtraInterfaces (_param : ContextParam) =
        [
            "    interface IFeature"
        ]

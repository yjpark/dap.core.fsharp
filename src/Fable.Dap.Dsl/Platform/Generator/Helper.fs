[<AutoOpen>]
module Dap.Platform.Generator.Helper

open Microsoft.FSharp.Quotations

open Dap.Prelude
open Dap.Context.Generator
open Dap.Context.Meta.Util
open Dap.Platform.Meta
open System

type G with
    static member PackInterface (name, meta : PackMeta) =
        PackParam.Create name
        |> generate (new Pack.InterfaceGenerator (meta))
    static member PackInterface (expr : Expr<PackMeta>) =
        let (name, meta) = unquotePropertyGetExpr expr
        G.PackInterface (name, meta)

type G with
    static member AppInterface (name, meta : AppMeta) =
        AppParam.Create name
        |> generate (new App.InterfaceGenerator (meta))
    static member AppInterface (expr : Expr<AppMeta>) =
        let (name, meta) = unquotePropertyGetExpr expr
        G.AppInterface (name, meta)

type G with
    static member AppClass (name, meta : AppMeta) =
        AppParam.Create name
        |> generate (new App.ClassGenerator (meta))
    static member AppClass (expr : Expr<AppMeta>) =
        let (name, meta) = unquotePropertyGetExpr expr
        G.AppClass (name, meta)

type G with
    static member App (name, meta : AppMeta) =
        [
            G.AppInterface (name, meta)
            [""]
            G.AppClass (name, meta)
        ]|> List.concat
    static member App (expr : Expr<AppMeta>) =
        let (name, meta) = unquotePropertyGetExpr expr
        G.App (name, meta)

type G with
    static member PlatformOpens =
        [
            "open Dap.Prelude"
            "open Dap.Context"
            "open Dap.Platform"
        ]
    static member PlatformBuilderOpens =
        [
            "open Dap.Prelude"
            "open Dap.Context"
            "open Dap.Context.Builder"
            "open Dap.Platform"
            //"open Dap.Platform.Builder"
        ]
    static member PackOpens =
        [
#if !FABLE_COMPILER
            "open System.Threading.Tasks"
            "open FSharp.Control.Tasks.V2"
#endif
        ] @ G.PlatformBuilderOpens
    static member AppOpens = G.PackOpens

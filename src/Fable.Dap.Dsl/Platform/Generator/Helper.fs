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
    static member AppInterface (name, meta : AppMeta, ?isGui : bool) =
        let isGui = defaultArg isGui false
        AppParam.Create name isGui
        |> generate (new App.InterfaceGenerator (meta))
    static member AppInterface (expr : Expr<AppMeta>, ?isGui : bool) =
        let (name, meta) = unquotePropertyGetExpr expr
        G.AppInterface (name, meta, ?isGui = isGui)
    static member GuiAppInterface (name, meta : AppMeta) =
        G.AppInterface (name, meta, isGui = true)
    static member GuiAppInterface (expr : Expr<AppMeta>) =
        G.AppInterface (expr, isGui = true)

type G with
    static member AppClass (name, meta : AppMeta, ?isGui : bool) =
        let isGui = defaultArg isGui false
        AppParam.Create name isGui
        |> generate (new App.ClassGenerator (meta))
    static member AppClass (expr : Expr<AppMeta>, ?isGui : bool) =
        let (name, meta) = unquotePropertyGetExpr expr
        G.AppClass (name, meta, ?isGui = isGui)
    static member GuiAppClass (name, meta : AppMeta) =
        G.AppClass (name, meta, isGui = true)
    static member GuiAppClass (expr : Expr<AppMeta>) =
        G.AppClass (expr, isGui = true)

type G with
    static member App (name, meta : AppMeta, ?isGui : bool) =
        [
            G.AppInterface (name, meta, ?isGui = isGui)
            [""]
            G.AppClass (name, meta, ?isGui = isGui)
        ]|> List.concat
    static member App (expr : Expr<AppMeta>, ?isGui : bool) =
        let (name, meta) = unquotePropertyGetExpr expr
        G.App (name, meta, ?isGui = isGui)
    static member GuiApp (name, meta : AppMeta) =
        G.App (name, meta, isGui = true)
    static member GuiApp (expr : Expr<AppMeta>) =
        G.App (expr, isGui = true)

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
            "open System.Threading"
            "open System.Threading.Tasks"
            "open FSharp.Control.Tasks.V2"
#endif
        ] @ G.PlatformBuilderOpens
    static member AppOpens = G.PackOpens

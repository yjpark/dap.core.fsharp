[<AutoOpen>]
module Dap.Platform.Generator.Helper

open Microsoft.FSharp.Quotations

open Dap.Prelude
open Dap.Context.Generator
open Dap.Context.Meta.Util
open Dap.Platform.Meta

type G with
    static member PackInterface (param : PackParam, meta : PackMeta) =
        new Pack.InterfaceGenerator (meta)
        :> IGenerator<PackParam>
        |> fun g -> g.Generate param
    static member PackInterface (name, meta) =
        let param = PackParam.Create name
        G.PackInterface (param, meta)
    static member PackInterface (expr : Expr<PackMeta>) =
        let (name, meta) = unquotePropertyGetExpr expr
        G.PackInterface (name, meta)
    static member AppInterface (param : AppParam, meta : AppMeta) =
        new App.InterfaceGenerator (meta)
        :> IGenerator<AppParam>
        |> fun g -> g.Generate param
    static member AppInterface (name, meta) =
        let param = AppParam.Create name
        G.AppInterface (param, meta)
    static member AppInterface (expr : Expr<AppMeta>) =
        let (name, meta) = unquotePropertyGetExpr expr
        G.AppInterface (name, meta)
    static member AppClass (param : AppParam, meta : AppMeta) =
        new App.ClassGenerator (meta)
        :> IGenerator<AppParam>
        |> fun g -> g.Generate param
    static member AppClass (name, meta : AppMeta) =
        let param = AppParam.Create name
        G.AppClass (param, meta)
    static member AppClass (expr : Expr<AppMeta>) =
        let (name, meta) = unquotePropertyGetExpr expr
        G.AppClass (name, meta)
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
            "open System.Threading.Tasks"
            "open FSharp.Control.Tasks.V2"
        ] @ G.PlatformBuilderOpens
    static member AppOpens = G.PackOpens

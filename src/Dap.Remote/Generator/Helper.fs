[<AutoOpen>]
module Dap.Remote.Generator.Helper

open Microsoft.FSharp.Quotations

open Dap.Prelude
open Dap.Context.Generator
open Dap.Context.Meta
open Dap.Context.Meta.Util
open Dap.Remote.Meta

type G with
    static member Stub (name, meta : StubMeta) =
        new Stub.StubGenerator (meta)
        :> IGenerator<StubParam>
        |> fun g -> g.Generate (StubParam.Create name)
    static member Stub (expr) =
        let (name, meta) = unquotePropertyGetExpr expr
        G.Stub (name, meta)

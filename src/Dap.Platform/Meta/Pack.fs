module Dap.Platform.Meta.Pack

open Microsoft.FSharp.Quotations

open Dap.Context.Meta
open Dap.Context.Meta.Util

type Builder (parents : (string * PackMeta) list) =
    inherit MetaBuilder<PackMeta> ()
    new (parents' : Expr<PackMeta> list) =
        let parents' =
            parents'
            |> List.map unquotePropertyGetExpr
        Builder (parents')
    override __.Zero () = PackMeta.Create parents [] [] []
    [<CustomOperation("nothing")>]
    member __.Nothing (meta : PackMeta, ()) =
        meta
    [<CustomOperation("extra")>]
    member __.Extra (meta : PackMeta, args : ExtraArgsMeta) =
        {meta with ExtraArgs = meta.ExtraArgs @ [args]}
    [<CustomOperation("add")>]
    member __.Add (meta : PackMeta, service : AgentMeta) =
        {meta with Services = meta.Services @ [service]}
    [<CustomOperation("add_pack'")>]
    member this.AddPack' (meta : PackMeta, pack : string, service : AgentMeta) =
        this.Add (meta, {service with Pack = Some pack})
    [<CustomOperation("add_pack")>]
    member this.AddPack (meta : PackMeta, expr, service : AgentMeta) =
        let (name, _meta) = unquotePropertyGetExpr expr
        this.AddPack' (meta, name, service)

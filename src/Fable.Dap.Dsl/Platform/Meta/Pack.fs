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
#if !FABLE_COMPILER
//Note: Leave AgentMeta in fable, since server helpers might depend on spawner helpers
//also it' might be possible that want to support spawner for fable later.
//Just remove the custom operations here, so it's not possible to create spawners.
    [<CustomOperation("register")>]
    member __.Register (meta : PackMeta, spawner : AgentMeta) =
        {meta with Spawners = meta.Spawners @ [spawner]}
    [<CustomOperation("register_pack'")>]
    member this.RegisterPack' (meta : PackMeta, pack : string, spawner : AgentMeta) =
        this.Register (meta, {spawner with Pack = Some pack})
    [<CustomOperation("register_pack")>]
    member this.RegisterPack (meta : PackMeta, expr, spawner : AgentMeta) =
        let (name, _meta) = unquotePropertyGetExpr expr
        this.RegisterPack' (meta, name, spawner)
#endif
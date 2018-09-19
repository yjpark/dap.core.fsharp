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
    [<CustomOperation("add")>]
    member __.Add (meta : PackMeta, service : ServiceMeta) =
        {meta with Services = meta.Services @ [service]}
    [<CustomOperation("register")>]
    member __.Register (meta : PackMeta, spawner : SpawnerMeta) =
        {meta with Spawners = meta.Spawners @ [spawner]}
    [<CustomOperation("extra")>]
    member __.Extra (meta : PackMeta, args : ExtraArgsMeta) =
        {meta with ExtraArgs = meta.ExtraArgs @ [args]}
    [<CustomOperation("add_pack'")>]
    member this.AddPack' (meta : PackMeta, pack : string, service : ServiceMeta) =
        this.Add (meta, {service with Pack = Some pack})
    [<CustomOperation("register_pack'")>]
    member this.RegisterPack' (meta : PackMeta, pack : string, spawner : SpawnerMeta) =
        this.Register (meta, {spawner with Pack = Some pack})
    [<CustomOperation("add_pack")>]
    member this.AddPack (meta : PackMeta, expr, service : ServiceMeta) =
        let (name, _meta) = unquotePropertyGetExpr expr
        this.AddPack' (meta, name, service)
    [<CustomOperation("register_pack")>]
    member this.RegisterPack (meta : PackMeta, expr, spawner : SpawnerMeta) =
        let (name, _meta) = unquotePropertyGetExpr expr
        this.RegisterPack' (meta, name, spawner)

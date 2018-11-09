module Dap.Platform.Meta.App

open Microsoft.FSharp.Quotations

open Dap.Context.Meta
open Dap.Context.Meta.Util

type Builder (clock : string) =
    inherit MetaBuilder<AppMeta> ()
    override __.Zero () = AppMeta.Create clock None []
    [<CustomOperation("platform")>]
    member __.Platform (meta : AppMeta, platform : string) =
        {meta with Platform = Some platform}
    [<CustomOperation("has'")>]
    member __.Has' (meta : AppMeta, packName : string, packMeta : PackMeta) =
        {meta with Packs = meta.Packs @ [(packName, packMeta)]}
    [<CustomOperation("has")>]
    member this.Has (meta : AppMeta, expr : Expr<PackMeta>) =
        let (packName, packMeta) = unquotePropertyGetExpr expr
        this.Has' (meta, packName, packMeta)


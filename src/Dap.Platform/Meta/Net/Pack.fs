[<AutoOpen>]
module Dap.Platform.Meta.Net.Pack

open Microsoft.FSharp.Quotations

open Dap.Context.Meta
open Dap.Context.Meta.Util
open Dap.Platform.Meta

type Dap.Platform.Meta.Pack.Builder with
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
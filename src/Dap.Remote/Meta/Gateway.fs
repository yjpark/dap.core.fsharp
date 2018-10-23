[<AutoOpen>]
module Dap.Remote.Meta.Gateway

open Dap.Prelude
open Dap.Context
open Dap.Context.Meta
open Dap.Platform
open Dap.Platform.Meta

type M with
    static member gatewaySpawner (aliases : ModuleAlias list, reqEvt : string, hubSpec : string, ?logTraffic : bool, ?kind : Kind) =
        let logTraffic = defaultArg logTraffic true
        let kind = defaultArg kind Dap.Remote.WebSocketGateway.Gateway.Kind
        let alias = "Gateway", "Dap.Remote.WebSocketGateway.Gateway"
        let args = sprintf "Gateway.Args<%s>" reqEvt
        let args =
            sprintf "(Gateway.args %s %b)" hubSpec logTraffic
            |> CodeArgs args
        let type' = "Gateway.Gateway"
        let spec = "Dap.Remote.WebSocketGateway.Logic.spec"
        M.spawner (alias :: aliases, args, type', spec, kind)
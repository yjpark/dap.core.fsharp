[<AutoOpen>]
module Dap.Remote.Meta.Proxy

open Dap.Prelude
open Dap.Context
open Dap.Context.Meta
open Dap.Platform
open Dap.Platform.Meta

type M with
    static member proxyService (aliases : ModuleAlias list, reqResEvt : string, stubSpec : string, url : string, retryDelay : float<second> option, logTraffic : bool, kind : Kind, key : Key) =
        let retryDelay =
            retryDelay
            |> Option.map (fun d -> sprintf "(Some %f<second>)" d)
            |> Option.defaultValue "None"
        let alias = "Proxy", "Dap.Remote.WebSocketProxy.Proxy"
        let args = sprintf "Proxy.Args<%s>" reqResEvt
        let args =
            sprintf "(Proxy.args %s %s %s %b)" stubSpec url retryDelay logTraffic
            |> CodeArgs args
        let type' = sprintf "Proxy.Proxy<%s>" reqResEvt
        let spec = "Dap.Remote.Proxy.Logic.Logic.spec"
        M.service (alias :: aliases, args, type', spec, kind, key)
    static member proxyService (aliases : ModuleAlias list, reqResEvt : string, stubSpec : string, url : string, retryDelay : float<second> option, logTraffic : bool, key : Key) =
        M.proxyService (aliases, reqResEvt, stubSpec, url, retryDelay, logTraffic, Dap.Remote.WebSocketProxy.Proxy.Kind, key)
    static member proxyService (aliases : ModuleAlias list, reqResEvt : string, stubSpec : string, url : string, retryDelay : float<second> option, logTraffic : bool) =
        M.proxyService (aliases, reqResEvt, stubSpec, url, retryDelay, logTraffic, NoKey)

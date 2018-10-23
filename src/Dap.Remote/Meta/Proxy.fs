[<AutoOpen>]
module Dap.Remote.Meta.Proxy

open Dap.Prelude
open Dap.Context
open Dap.Context.Meta
open Dap.Platform
open Dap.Platform.Meta

type M with
    static member packetClientSpawner (?logTraffic : bool, ?bufferSize : int, ?refreshInterval : Duration) =
        let logTraffic = defaultArg logTraffic true
        let bufferSize = defaultArg bufferSize Dap.WebSocket.Const.DefaultBufferSize
        let refreshInterval = defaultArg refreshInterval Dap.WebSocket.Const.DefaultRefreshInterval
        let refreshInterval = jsonInitValue "Duration" E.duration refreshInterval
        let alias = "PacketClient", "Dap.Remote.WebSocketProxy.PacketClient"
        let args = "PacketClient.Args"
        let args =
            sprintf "(PacketClient.args %b %i %s)" logTraffic bufferSize refreshInterval
            |> CodeArgs args
        let type' = "PacketClient.Agent"
        let spec = "Dap.WebSocket.Internal.Logic.spec"
        let kind = Dap.Remote.WebSocketProxy.PacketClient.Kind
        M.spawner ([alias], args, type', spec, kind)

type M with
    static member packetConnSpawner (?logTraffic : bool, ?bufferSize : int, ?refreshInterval : Duration) =
        let logTraffic = defaultArg logTraffic true
        let bufferSize = defaultArg bufferSize Dap.WebSocket.Const.DefaultBufferSize
        let refreshInterval = defaultArg refreshInterval Dap.WebSocket.Const.DefaultRefreshInterval
        let refreshInterval = jsonInitValue "Duration" E.duration refreshInterval
        let alias = "PacketConn", "Dap.Remote.WebSocketGateway.PacketConn"
        let args = "PacketConn.Args"
        let args =
            sprintf "(PacketConn.args %b %i %s)" logTraffic bufferSize refreshInterval
            |> CodeArgs args
        let type' = "PacketConn.Agent"
        let spec = "Dap.WebSocket.Internal.Logic.spec"
        let kind = Dap.Remote.WebSocketGateway.PacketConn.Kind
        M.spawner ([alias], args, type', spec, kind)

type M with
    static member proxySpawner (aliases : ModuleAlias list, reqResEvt : string, stubSpec : string, url : string, retryDelay : float<second> option, ?logTraffic : bool, ?kind : Kind) =
        let logTraffic = defaultArg logTraffic true
        let kind = defaultArg kind Dap.Remote.WebSocketProxy.Proxy.Kind
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
        M.spawner (alias :: aliases, args, type', spec, kind)
    static member proxyService (aliases : ModuleAlias list, reqResEvt : string, stubSpec : string, url : string, retryDelay : float<second> option, ?logTraffic : bool, ?kind : Kind, ?key : Key) =
        let key = defaultArg key NoKey
        M.proxySpawner (aliases, reqResEvt, stubSpec, url, retryDelay, ?logTraffic = logTraffic, ?kind = kind)
        |> fun s -> s.ToService key

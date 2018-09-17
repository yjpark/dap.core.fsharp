module Dap.Remote.Meta

open Dap.Prelude
open Dap.Context
open Dap.Context.Meta
open Dap.Platform
open Dap.Platform.Meta

type M with
    static member packetClientSpawner (logTraffic : bool, bufferSize : int, kind : Kind) =
        let alias = "PacketClient", "Dap.Remote.WebSocketProxy.PacketClient"
        let args = "PacketClient.Args"
        let args =
            sprintf "(PacketClient.args %b %i)" logTraffic bufferSize
            |> CodeArgs args
        let type' = "PacketClient.Agent"
        let spec = "Dap.WebSocket.Internal.Logic.spec"
        M.spawner ([alias], args, type', spec, kind)
    static member packetClientSpawner (logTraffic : bool, kind : Kind) =
        M.packetClientSpawner(logTraffic, Dap.WebSocket.Const.DefaultBufferSize, kind)
    static member packetClientSpawner (logTraffic : bool) =
        M.packetClientSpawner(logTraffic, Dap.Remote.WebSocketProxy.PacketClient.Kind)

type M with
    static member packetConnSpawner (logTraffic : bool, bufferSize : int, kind : Kind) =
        let alias = "PacketConn", "Dap.Remote.WebSocketGateway.PacketConn"
        let args = "PacketConn.Args"
        let args =
            sprintf "(PacketConn.args %b %i)" logTraffic bufferSize
            |> CodeArgs args
        let type' = "PacketConn.Agent"
        let spec = "Dap.WebSocket.Internal.Logic.spec"
        M.spawner ([alias], args, type', spec, kind)
    static member packetConnSpawner (logTraffic : bool, kind : Kind) =
        M.packetConnSpawner(logTraffic, Dap.WebSocket.Const.DefaultBufferSize, kind)
    static member packetConnSpawner (logTraffic : bool) =
        M.packetConnSpawner(logTraffic, Dap.Remote.WebSocketGateway.PacketConn.Kind)

type M with
    static member proxySpawner (aliases : ModuleAlias list, reqResEvt : string, stubSpec : string, url : string, retryDelay : float<second> option, logTraffic : bool, kind : Kind) =
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
        let spec = "Dap.Remote.Proxy.Logic.spec"
        M.spawner (alias :: aliases, args, type', spec, kind)
    static member proxyService (aliases : ModuleAlias list, reqResEvt : string, stubSpec : string, url : string, retryDelay : float<second> option, logTraffic : bool, kind : Kind, key : Key) =
        M.proxySpawner (aliases, reqResEvt, stubSpec, url, retryDelay, logTraffic, kind)
        |> fun s -> s.ToService key
    static member proxyService (aliases : ModuleAlias list, reqResEvt : string, stubSpec : string, url : string, retryDelay : float<second> option, logTraffic : bool, kind : Kind) =
        M.proxyService (aliases, reqResEvt, stubSpec, url, retryDelay, logTraffic, kind, NoKey)

type M with
    static member gatewaySpawner (aliases : ModuleAlias list, reqEvt : string, hubSpec : string, logTraffic : bool, kind : Kind) =
        let alias = "Gateway", "Dap.Remote.WebSocketGateway.Gateway"
        let args = sprintf "Gateway.Args<%s>" reqEvt
        let args =
            sprintf "(Gateway.args %s %b)" hubSpec logTraffic
            |> CodeArgs args
        let type' = "Gateway.Gateway"
        let spec = "Dap.Remote.WebSocketGateway.Logic.spec"
        M.spawner (alias :: aliases, args, type', spec, kind)

[<AutoOpen>]
module Dap.Remote.Meta.Net.Proxy

open Dap.Prelude
open Dap.Context
open Dap.Context.Meta
open Dap.Platform
open Dap.Platform.Meta

type M with
    static member packetClient (?logTraffic : bool, ?bufferSize : int, ?refreshInterval : Duration, ?key : Key) =
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
        M.agent (args, type', spec, kind, ?key = key, aliases = [alias])

type M with
    static member packetConn (?logTraffic : bool, ?bufferSize : int, ?refreshInterval : Duration, ?key : Key) =
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
        M.agent (args, type', spec, kind, ?key = key, aliases = [alias])

type M with
    static member proxy (reqResEvt : string, stubSpec : string, url : string, ?autoConnect : bool, ?retryDelay : float<second>, ?logTraffic : bool, ?kind : Kind, ?key : Key, ?aliases : ModuleAlias list) =
        let autoConnect = defaultArg autoConnect true
        let logTraffic = defaultArg logTraffic true
        let kind = defaultArg kind Dap.Remote.WebSocketProxy.Proxy.Kind
        let retryDelay =
            retryDelay
            |> Option.map (fun d -> sprintf "(Some %f<second>)" d)
            |> Option.defaultValue "None"
        let aliases = defaultArg aliases []
        let alias = "Proxy", "Dap.Remote.WebSocketProxy.Proxy"
        let args = sprintf "Proxy.Args<%s>" reqResEvt
        let args =
            sprintf "(Proxy.args %s %s %b %s %b)" stubSpec url autoConnect retryDelay logTraffic
            |> CodeArgs args
        let type' = sprintf "Proxy.Proxy<%s>" reqResEvt
        let spec = "Dap.Remote.Proxy.Logic.Logic.spec"
        M.agent (args, type', spec, kind, ?key = key, aliases = alias :: aliases)
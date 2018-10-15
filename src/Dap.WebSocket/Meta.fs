module Dap.WebSocket.Meta

open Dap.Prelude
open Dap.Context
open Dap.Context.Meta
open Dap.Platform
open Dap.Platform.Meta

type M with
    static member textClientSpawner (encoding : string, logTraffic : bool, bufferSize : int, refreshInterval : Duration, kind : Kind) =
        let refreshInterval = jsonInitValue "Duration" E.duration refreshInterval
        let alias = "TextClient", "Dap.WebSocket.Client.TextClient"
        let args =
            sprintf "(TextClient.args %s %b %i %s)" encoding logTraffic bufferSize refreshInterval
            |> CodeArgs "TextClient.Args"
        let type' = "TextClient.Agent"
        let spec = "Dap.WebSocket.Internal.Logic.spec"
        M.spawner ([alias], args, type', spec, kind)
    static member utf8TextClientSpawner (logTraffic : bool, bufferSize : int, refreshInterval : Duration, kind : Kind) =
        M.textClientSpawner ("System.Text.Encoding.UTF8", logTraffic, bufferSize, refreshInterval, kind)
    static member utf8TextClientSpawner (logTraffic : bool, kind : Kind) =
        M.utf8TextClientSpawner(logTraffic, Dap.WebSocket.Const.DefaultBufferSize, Dap.WebSocket.Const.DefaultRefreshInterval, kind)
    static member utf8TextClientSpawner (logTraffic : bool) =
        M.utf8TextClientSpawner(logTraffic, Dap.WebSocket.Client.TextClient.Utf8Kind)

type M with
    static member textConnSpawner (encoding : string, logTraffic : bool, bufferSize : int, refreshInterval : Duration, kind : Kind) =
        let refreshInterval = jsonInitValue "Duration" E.duration refreshInterval
        let alias = "TextConn", "Dap.WebSocket.Conn.TextConn"
        let args = "TextConn.Args"
        let args =
            sprintf "(TextConn.args %s %b %i %s)" encoding logTraffic bufferSize refreshInterval
            |> CodeArgs args
        let type' = "TextConn.Agent"
        let spec = "Dap.WebSocket.Internal.Logic.spec"
        M.spawner ([alias], args, type', spec, kind)
    static member utf8TextConnSpawner (logTraffic : bool, bufferSize : int, refreshInterval : Duration, kind : Kind) =
        M.textConnSpawner ("System.Text.Encoding.UTF8", logTraffic, bufferSize, refreshInterval, kind)
    static member utf8TextConnSpawner (logTraffic : bool, kind : Kind) =
        M.utf8TextConnSpawner(logTraffic, Dap.WebSocket.Const.DefaultBufferSize, Dap.WebSocket.Const.DefaultRefreshInterval, kind)
    static member utf8TextConnSpawner (logTraffic : bool) =
        M.utf8TextConnSpawner(logTraffic, Dap.WebSocket.Conn.TextConn.Utf8Kind)

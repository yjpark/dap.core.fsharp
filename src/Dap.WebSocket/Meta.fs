module Dap.WebSocket.Meta

open Dap.Prelude
open Dap.Context
open Dap.Context.Meta
open Dap.Platform.Meta

type M with
    static member textClientSpawner (encoding : string, logTraffic : bool, bufferSize : int, kind : Kind) =
        let alias = "TextClient", "Dap.WebSocket.Client.TextClient"
        let args =
            sprintf "(TextClient.args %s %b %i)" encoding logTraffic bufferSize
            |> CodeArgs "TextClient.Args"
        let type' = "TextClient.Agent"
        let spec = "Dap.WebSocket.Internal.Logic.spec"
        M.spawner ([alias], args, type', spec, kind)
    static member utf8TextClientSpawner (logTraffic : bool, bufferSize : int, kind : Kind) =
        M.textClientSpawner ("System.Text.Encoding.UTF8", logTraffic, bufferSize, kind)
    static member utf8TextClientSpawner (logTraffic : bool, kind : Kind) =
        M.utf8TextClientSpawner(logTraffic, Dap.WebSocket.Const.DefaultBufferSize, kind)
    static member utf8TextClientSpawner (logTraffic : bool) =
        M.utf8TextClientSpawner(logTraffic, Dap.WebSocket.Client.TextClient.Utf8Kind)

type M with
    static member textConnSpawner (encoding : string, logTraffic : bool, bufferSize : int, kind : Kind) =
        let alias = "TextConn", "Dap.WebSocket.Conn.TextConn"
        let args = "TextConn.Args"
        let args =
            sprintf "(TextConn.args %s %b %i)" encoding logTraffic bufferSize
            |> CodeArgs args
        let type' = "TextConn.Agent"
        let spec = "Dap.WebSocket.Internal.Logic.spec"
        M.spawner ([alias], args, type', spec, kind)
    static member utf8TextConnSpawner (logTraffic : bool, bufferSize : int, kind : Kind) =
        M.textConnSpawner ("System.Text.Encoding.UTF8", logTraffic, bufferSize, kind)
    static member utf8TextConnSpawner (logTraffic : bool, kind : Kind) =
        M.utf8TextConnSpawner(logTraffic, Dap.WebSocket.Const.DefaultBufferSize, kind)
    static member utf8TextConnSpawner (logTraffic : bool) =
        M.utf8TextConnSpawner(logTraffic, Dap.WebSocket.Conn.TextConn.Utf8Kind)

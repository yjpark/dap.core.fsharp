module Dap.WebSocket.Meta

open Dap.Prelude
open Dap.Context
open Dap.Context.Meta
open Dap.Platform
open Dap.Platform.Meta

type M with
    static member textClient (encoding : string, kind : Kind, ?logTraffic : bool, ?bufferSize : int, ?refreshInterval : Duration, ?key : Key) =
        let logTraffic = defaultArg logTraffic true
        let bufferSize = defaultArg bufferSize Dap.WebSocket.Const.DefaultBufferSize
        let refreshInterval = defaultArg refreshInterval Dap.WebSocket.Const.DefaultRefreshInterval
        let refreshInterval = jsonInitValue "Duration" E.duration refreshInterval
        let alias = "TextClient", "Dap.WebSocket.Client.TextClient"
        let args =
            sprintf "(TextClient.args %s %b %i %s)" encoding logTraffic bufferSize refreshInterval
            |> CodeArgs "TextClient.Args"
        let type' = "TextClient.Agent"
        let spec = "Dap.WebSocket.Internal.Logic.spec"
        M.agent (args, type', spec, kind, ?key = key, aliases = [alias])
    static member utf8TextClient (?logTraffic : bool, ?bufferSize : int, ?refreshInterval : Duration, ?kind : Kind, ?key : Key) =
        let kind = defaultArg kind Dap.WebSocket.Client.TextClient.Utf8Kind
        M.textClient ("System.Text.Encoding.UTF8", kind, ?logTraffic = logTraffic, ?bufferSize = bufferSize, ?refreshInterval = refreshInterval, ?key = key)

type M with
    static member textConn (encoding : string, kind : Kind, ?logTraffic : bool, ?bufferSize : int, ?refreshInterval : Duration, ?key : Key) =
        let logTraffic = defaultArg logTraffic true
        let bufferSize = defaultArg bufferSize Dap.WebSocket.Const.DefaultBufferSize
        let refreshInterval = defaultArg refreshInterval Dap.WebSocket.Const.DefaultRefreshInterval
        let refreshInterval = jsonInitValue "Duration" E.duration refreshInterval
        let alias = "TextConn", "Dap.WebSocket.Conn.TextConn"
        let args = "TextConn.Args"
        let args =
            sprintf "(TextConn.args %s %b %i %s)" encoding logTraffic bufferSize refreshInterval
            |> CodeArgs args
        let type' = "TextConn.Agent"
        let spec = "Dap.WebSocket.Internal.Logic.spec"
        M.agent (args, type', spec, kind, ?key = key, aliases = [alias])
    static member utf8TextConn (?logTraffic : bool, ?bufferSize : int, ?refreshInterval : Duration, ?kind : Kind, ?key : Key) =
        let kind = defaultArg kind Dap.WebSocket.Conn.TextConn.Utf8Kind
        M.textConn ("System.Text.Encoding.UTF8", kind, ?logTraffic = logTraffic, ?bufferSize = bufferSize, ?refreshInterval = refreshInterval, ?key = key)

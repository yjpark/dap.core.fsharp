module Dap.WebSocket.Meta

open Dap.Prelude
open Dap.Context
open Dap.Context.Meta
open Dap.Platform.Meta

type M with
    static member textClient (uri : string, ?logTraffic : bool, ?kind : Kind, ?key : Key) =
        let logTraffic = defaultArg logTraffic true
        let kind = defaultArg kind Dap.WebSocket.Client.TextClient.Kind
        let alias = "TextClient", "Dap.WebSocket.Client.TextClient"
        let args =
            sprintf "(TextClient.args %s %b)" uri logTraffic
            |> CodeArgs "TextClient.Args"
        let type' = "TextClient.Agent"
        let spec = "Dap.WebSocket.Client.Logic.spec"
        M.agent (args, type', spec, kind, ?key = key, aliases = [alias])
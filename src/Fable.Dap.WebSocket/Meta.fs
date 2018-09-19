module Dap.WebSocket.Meta

open Dap.Prelude
open Dap.Context
open Dap.Context.Meta
open Dap.Platform.Meta

type M with
    static member textClientService (uri : string, logTraffic : bool, kind : Kind, key : Key) =
        let alias = "TextClient", "Dap.WebSocket.Client.TextClient"
        let args =
            sprintf "(TextClient.args %s %b)" uri logTraffic
            |> CodeArgs "TextClient.Args"
        let type' = "TextClient.Agent"
        let spec = "Dap.WebSocket.Client.Logic.spec"
        M.service ([alias], args, type', spec, kind)
    static member textClientService (uri : string, logTraffic : bool, key : Key) =
        M.textClientService(uri, logTraffic, Dap.WebSocket.Client.TextClient.Kind, key)
    static member textClientService (uri : string, logTraffic : bool) =
        M.textClientService(uri, logTraffic, NoKey)
[<RequireQualifiedAccess>]
module Dap.Remote.WebSocketService.Agent

open Dap.Platform
open Dap.Remote
open Dap.Remote.WebSocketService
open Dap.Remote.WebSocketService.Types

let getSpec (hubSpec : HubSpec<'req, 'evt>) (logTraffic : bool) =
    fun _agent ->
        {
            HubSpec = hubSpec
            LogTraffic = logTraffic
        }
    |> Logic.getSpec

let getSpawner env hubSpec logTraffic =
    getSpec hubSpec logTraffic
    |> Agent.getSpawner env
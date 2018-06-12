[<RequireQualifiedAccess>]
module Dap.Remote.WebSocketService.Agent

open Dap.Platform
open Dap.Remote
open Dap.Remote.WebSocketService
open Dap.Remote.WebSocketService.Types

let getSpec (hubSpec : HubSpec<'req, 'evt>) (logTraffic : bool) =
    fun () ->
        {
            HubSpec = hubSpec
            LogTraffic = logTraffic
            InternalEvent' = new Event<InternalEvt<'evt>>()
        }
    |> Logic.getSpec

let getSpawner env hubSpec logTraffic =
    getSpec hubSpec logTraffic
    |> Agent.getSpawner env
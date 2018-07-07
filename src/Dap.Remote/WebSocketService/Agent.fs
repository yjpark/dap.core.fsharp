[<RequireQualifiedAccess>]
module Dap.Remote.WebSocketService.Agent

open Dap.Platform
open Dap.Remote
open Dap.Remote.WebSocketService
open Dap.Remote.WebSocketService.Types

type Agent = IAgent<Req, NoEvt>

let spec (hubSpec : HubSpec<'req, 'evt>) (logTraffic : bool) =
    {
        HubSpec = hubSpec
        LogTraffic = logTraffic
    }
    |> Logic.spec

let registerAsync kind hubSpec logTraffic env =
    let spec = spec hubSpec logTraffic
    env |> Env.registerAsync spec kind

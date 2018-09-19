[<RequireQualifiedAccess>]
module Dap.Remote.WebSocketGateway.Gateway

open Dap.Platform
open Dap.Remote
module GatewayTypes = Dap.Remote.WebSocketGateway.Types

[<Literal>]
let Kind = "WebSocketGateway"

type Args<'req, 'evt when 'req :> IReq and 'evt :> IEvt> = GatewayTypes.Args<'req, 'evt>
type Gateway = IAgent<GatewayTypes.Req, NoEvt>

let args (hubSpec : HubSpec<'req, 'evt>) (logTraffic : bool) =
    Args<'req, 'evt>.Create hubSpec logTraffic

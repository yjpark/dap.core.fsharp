[<RequireQualifiedAccess>]
module Dap.Remote.WebSocketProxy.Agent

open Dap.Platform
open Dap.Remote
open Dap.Remote.WebSocketProxy.Types

type Agent<'req, 'evt> when 'req :> IRequest and 'evt :> IEvent =
    IAgent<'req, 'evt>

let create'<'req, 'res, 'evt when 'req :> IRequest and 'evt :> IEvent> (spec : StubSpec<'res, 'evt>) (key : string) (uri : string) (logTraffic : bool) : Dap.Remote.WebSocketProxy.Types.Agent<'req, 'res, 'evt> =
    fun agent ->
        {
            Spec = spec
            Uri = uri
            LogTraffic = logTraffic
            ResponseEvent' = new Bus<'res> (agent)
        }
    |> Logic.getSpec
    |> Agent.create Kind key

let create<'req, 'res, 'evt when 'req :> IRequest and 'evt :> IEvent> (spec : StubSpec<'res, 'evt>) (key : string) (uri : string) (logTraffic : bool) =
    create'<'req, 'res, 'evt> spec key uri logTraffic
    :> Agent<'req, 'evt>
[<RequireQualifiedAccess>]
module Dap.Remote.WebSocketProxy.Proxy

open Dap.Platform
open Dap.Remote
open Dap.Remote.WebSocketProxy.Types

type Proxy<'req, 'res, 'evt when 'req :> IRequest and 'evt :> IEvent> (agent' : Agent<'req, 'res, 'evt>) =
    let agent = agent'
    member _this.Agent = agent
    member _this.Actor = agent.Actor
    interface IProxy<'req, 'res, 'evt> with
        member _this.Post (req : 'req) = agent.Post req
        member _this.OnEvent = agent.Actor.OnEvent
        member _this.OnResponse = agent.Actor.Args.OnResponse
        member _this.Connected = agent.Actor.State.Connected

let create'<'req, 'res, 'evt when 'req :> IRequest and 'evt :> IEvent> (spec : StubSpec<'res, 'evt>) (key : string) (uri : string) (logTraffic : bool) : Proxy<'req, 'res, 'evt> =
    let agent = Dap.Remote.WebSocketProxy.Agent.create' spec key uri logTraffic
    new Proxy<'req, 'res, 'evt> (agent)

let create<'req, 'res, 'evt when 'req :> IRequest and 'evt :> IEvent> (spec : StubSpec<'res, 'evt>) (key : string) (uri : string) (logTraffic : bool) =
    create'<'req, 'res, 'evt> spec key uri logTraffic
    :> IProxy<'req, 'res, 'evt>
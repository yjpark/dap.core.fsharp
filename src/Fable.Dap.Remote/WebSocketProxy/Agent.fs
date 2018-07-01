[<RequireQualifiedAccess>]
module Dap.Remote.WebSocketProxy.Actor

open Dap.Platform
open Dap.Remote
open Dap.Remote.WebSocketProxy.Types

module WebSocket = Dap.WebSocket.Client.Types
module WebSocketActor = Dap.WebSocket.Client.Actor

let create (spec : StubSpec<'res, 'evt>) (key : string) (uri : string) (logTraffic : bool) =
    fun agent ->
        {
            Spec = spec
            Uri = uri
            LogTraffic = logTraffic
            ResponseEvent' = new Bus<'res> (agent)
        }
    |> Logic.getSpec
    |> Agent.create Kind key

let getOnResponse (agent : Agent<'req, 'res, 'evt>) : IBus<'res> =
    agent.Actor.Args.OnResponse
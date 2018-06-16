[<RequireQualifiedAccess>]
module Dap.Remote.WebSocketProxy.Actor

open Dap.Platform
open Dap.Remote
open Dap.Remote.WebSocketProxy.Types

module WebSocket = Dap.WebSocket.Client.Types
module WebSocketActor = Dap.WebSocket.Client.Actor

let create (spec : StubSpec<'res, 'evt>) (key : string) (uri : string) (logTraffic : bool) =
    fun () ->
        {
            Spec = spec
            Uri = uri
            LogTraffic = logTraffic
            Event' = new Event<'evt>()
            ResponseEvent' = new Event<'res>()
            InternalEvent' = new Event<InternalEvt>()
        }
    |> Logic.getSpec
    |> Actor.create Kind key

let getOnResponse (actor : Actor<'req, 'res, 'evt>) : IEvent<'res> =
    (Option.get actor.State).Args.OnResponse
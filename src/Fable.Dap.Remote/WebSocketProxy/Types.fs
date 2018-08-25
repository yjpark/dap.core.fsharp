module Dap.Remote.WebSocketProxy.Types

open Dap.Prelude
open Dap.Context
open Dap.Platform
open Dap.Remote
open Dap.Remote.Internal
open Dap.Remote.Proxy.Types

module WebSocketTypes = Dap.WebSocket.Client.Types

type Extra = {
    Socket : WebSocketTypes.Agent<Packet> option
} with
    static member New () =
        {
            Socket = None
        }

type SubEvt =
    | SocketEvt of WebSocketTypes.Evt<Packet>
    | SetSocket of WebSocketTypes.Agent<Packet>
    | DoReconnect

type ActorOperate<'req, 'res, 'evt when 'req :> IRequest and 'evt :> IEvent> =
    ActorOperate<Extra, SubEvt, 'req, 'res, 'evt>

type Proxy<'req, 'res, 'evt when 'req :> IRequest and 'evt :> IEvent> =
    Proxy<Extra, SubEvt, 'req, 'res, 'evt>

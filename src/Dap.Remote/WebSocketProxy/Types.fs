module Dap.Remote.WebSocketProxy.Types

open System.Threading

open Dap.Prelude
open Dap.Platform
open Dap.Remote
open Dap.Remote.Internal
open Dap.Remote.Proxy.Types

module WebSocketTypes = Dap.WebSocket.Types
module WebSocketClientTypes = Dap.WebSocket.Client.Types

type Extra = {
    Socket : WebSocketClientTypes.Agent<Packet> option
    Cts : CancellationTokenSource
} with
    static member New () =
        {
            Socket = None
            Cts = new CancellationTokenSource ()
        }

type SubEvt =
    | SocketEvt of WebSocketTypes.Evt<Packet>
    | SetSocket of WebSocketClientTypes.Agent<Packet>
    | DoReconnect

type ActorOperate<'req, 'res, 'evt when 'req :> IRequest and 'evt :> IEvent> =
    ActorOperate<Extra, SubEvt, 'req, 'res, 'evt>

type Proxy<'req, 'res, 'evt when 'req :> IRequest and 'evt :> IEvent> =
    Proxy<Extra, SubEvt, 'req, 'res, 'evt>

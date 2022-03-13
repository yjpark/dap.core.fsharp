module Dap.Remote.WebSocketProxy.Tasks

open System.Threading
open System.Threading.Tasks
open System.Net.WebSockets

open Dap.Prelude
open Dap.Platform
open Dap.Remote.Internal
open Dap.Remote.Proxy.Types
open Dap.Remote.WebSocketProxy.Types

module WebSocketClientTypes = Dap.WebSocket.Client.Types

let internal setSocketAsync : GetTask<Proxy<'req, 'res, 'evt>, unit> =
    fun runner -> task {
        let socketKey = sprintf "%s%s" runner.Ident.Kind runner.Ident.Key
        let! (socket, _) = runner.Env.HandleAsync <| DoGetAgent PacketClient.Kind socketKey
        let socket = socket :?> WebSocketClientTypes.Agent<Packet>
        runner.Deliver <| SubEvt ^<| SetSocket socket
    }

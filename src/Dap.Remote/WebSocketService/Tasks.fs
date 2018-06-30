module Dap.Remote.WebSocketService.Tasks

open System.Threading
open System.Threading.Tasks
open System.Net.WebSockets
open FSharp.Control.Tasks
open Dap.Prelude
open Dap.Platform
open Dap.Remote.WebSocketService.Types
module WebSocket = Dap.WebSocket.Conn.Types

let internal setSocketAsync : GetTask<Agent<'req, 'evt>, unit> =
    fun runner -> task {
        let socketKey = sprintf "%s_%s" runner.Ident.Kind runner.Ident.Key
        let! (socket, _) = runner.Env.HandleAsync <| DoGetAgent' PacketConn.Kind socketKey
        let socket = socket :?> PacketConn.Agent
        runner.Actor.Args.FireInternalEvent' <| SetSocket socket
    }

let internal doAttachAsync (ident : string) (token : CancellationToken)
                            (socket : WebSocket) : GetReplyTask<Agent<'req, 'evt>, Task> =
    fun msg callback runner -> task {
        while Option.isNone runner.Actor.State.Socket do
            do! Task.Delay 20
        let socket' = Option.get runner.Actor.State.Socket
        let! task = socket'.PostAsync <| WebSocket.DoConnect' ident token socket
        reply runner callback <| ack msg task
    }

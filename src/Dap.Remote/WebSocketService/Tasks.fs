module Dap.Remote.WebSocketService.Tasks

open System.Threading
open System.Threading.Tasks
open System.Net.WebSockets
open FSharp.Control.Tasks
open Dap.Prelude
open Dap.Platform
open Dap.Remote.WebSocketService.Types
module WebSocket = Dap.WebSocket.Conn.Types

let internal setSocketAsync (state : State<'req, 'evt>) : GetTask'<IAgent, unit> =
    fun runner -> task {
        let agent = runner.Self
        let socketKey = sprintf "%s_%s" agent.Ident.Kind agent.Ident.Key
        let! (socket, _) = agent.Env.HandleAsync <| DoGetAgent' PacketConn.Kind socketKey
        let socket = socket :?> PacketConn.Agent
        state.Socket <- Some socket
        socket.Actor.OnEvent.Add(state.Args.FireInternalEvent' << SocketEvt)
    }

let internal doAttachAsync (state : State<'req, 'evt>)
                            (ident : string) (token : CancellationToken)
                            (socket : WebSocket) : GetReplyTask<Task> =
    fun msg callback runner -> task {
        while Option.isNone state.Socket do
            do! Task.Delay 20
        let socket' = Option.get state.Socket
        let! task = socket'.PostAsync <| WebSocket.DoConnect' ident token socket
        reply runner callback <| ack msg task
    }

module Dap.WebSocket.Conn.Tasks

open Dap.Platform
open Dap.WebSocket
open Dap.WebSocket.Conn.Types

module BaseTasks = Dap.WebSocket.Internal.Tasks

let internal doReceiveFailed (state : State<'pkt>) : OnFailed<Agent<'pkt>>  =
    BaseTasks.doReceiveFailed OnDisconnected state

let internal doReceiveAsync (state : State<'pkt>) : GetTask<Agent<'pkt>, unit> =
    BaseTasks.doReceiveAsync OnReceived OnDisconnected state

let internal doSendAsync (state : State<'pkt>) (pkt : 'pkt) : GetReplyTask<Agent<'pkt>, SendStats> =
    BaseTasks.doSendAsync OnSent state pkt
[<RequireQualifiedAccess>]
module Dap.Remote.WebSocketProxy.PacketClient

open System.Text
open System.Net.WebSockets

open Dap.Prelude
open Dap.Platform
open Dap.Remote
open Dap.Remote.Internal
open Dap.WebSocket
module BaseTypes = Dap.WebSocket.Types
module ClientTypes = Dap.WebSocket.Client.Types

[<Literal>]
let Kind = "PacketClient"

type Req = ClientTypes.Req<Packet>
type Evt = BaseTypes.Evt<Packet>
type Args = BaseTypes.Args<ClientWebSocket, Packet, Req>
type Agent = ClientTypes.Agent<Packet>

let encode = Dap.Remote.WebSocketGateway.PacketConn.encode
let decode = Dap.Remote.WebSocketGateway.PacketConn.decode

let args logTraffic bufferSize refreshInterval =
    Args.Create logTraffic WebSocketMessageType.Text bufferSize refreshInterval encode decode Dap.WebSocket.Client.Logic.handleReq
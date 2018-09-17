[<RequireQualifiedAccess>]
module Dap.Remote.WebSocketGateway.PacketConn

open System.Text
open System.Net.WebSockets

open Dap.Prelude
open Dap.Context
open Dap.Platform
open Dap.Remote
open Dap.Remote.Internal
open Dap.WebSocket
module BaseTypes = Dap.WebSocket.Types
module ConnTypes = Dap.WebSocket.Conn.Types

[<Literal>]
let Kind = "PacketConn"

type Req = ConnTypes.Req<Packet>
type Evt = BaseTypes.Evt<Packet>
type Args = BaseTypes.Args<WebSocket, Packet, Req>
type Agent = ConnTypes.Agent<Packet>

let encode : Encode<Packet> =
    fun pkt ->
        Dap.WebSocket.Internal.Text.encode Encoding.UTF8 <| pkt.EncodeJson 0

let decode : Decode<Packet> =
    fun (buffer, index, count) ->
        Dap.WebSocket.Internal.Text.decode Encoding.UTF8 (buffer, index, count)
        |> decodeJson Packet.JsonDecoder

let args logTraffic bufferSize =
    Args.Create logTraffic WebSocketMessageType.Text bufferSize encode decode Dap.WebSocket.Conn.Logic.handleReq

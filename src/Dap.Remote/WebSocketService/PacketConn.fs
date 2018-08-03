[<RequireQualifiedAccess>]
module Dap.Remote.WebSocketService.PacketConn

open System.Text
open System.Net.WebSockets

open Dap.Prelude
open Dap.Platform
open Dap.Remote
open Dap.Remote.Internal
open Dap.WebSocket
open Dap.WebSocket.Conn.Types

[<Literal>]
let Kind = "WebSocketPacketConn"

type Agent = IAgent<Req<Packet>, Evt<Packet>>

type Evt = Evt<Packet>

let encode : Encode<Packet> =
    fun pkt ->
        Dap.WebSocket.Internal.Text.encode Encoding.UTF8 <| pkt.EncodeJson 0

let decode : Decode<Packet> =
    fun (buffer, index, count) ->
        Dap.WebSocket.Internal.Text.decode Encoding.UTF8 (buffer, index, count)
        |> decodeJson Packet.JsonDecoder

let spec logTraffic bufferSize =
    Dap.WebSocket.Conn.Logic.spec WebSocketMessageType.Text encode decode logTraffic bufferSize

let registerAsync' kind logTraffic bufferSize env =
    let spec = spec logTraffic bufferSize
    env |> Env.registerAsync spec kind

let registerAsync a b = registerAsync' Kind a b

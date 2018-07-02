[<RequireQualifiedAccess>]
module Dap.Remote.WebSocketService.PacketConn

open System.Text
open System.Net.WebSockets
open Dap.Platform
open Dap.Remote
open Dap.WebSocket
open Dap.WebSocket.Conn.Types

[<Literal>]
let Kind = "WebSocketPacketConn"

type Agent = IAgent<Req<Packet'>, Evt<Packet'>>

type Evt = Evt<Packet'>

let encode : Encode<Packet'> =
    fun pkt ->
        Dap.WebSocket.Internal.Text.encode Encoding.UTF8 <| Packet.encode pkt

let decode : Decode<Packet'> =
    fun (buffer, index, count) ->
        Packet.decode <| Dap.WebSocket.Internal.Text.decode Encoding.UTF8 (buffer, index, count)

let getSpawner env logTraffic bufferSize =
    Dap.WebSocket.Conn.Logic.getSpec encode decode logTraffic bufferSize
    |> Agent.getSpawner env
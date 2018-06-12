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

type Agent = IAgent<Model<Packet'>, Req<Packet'>, Evt<Packet'>>

type Event = Evt<Packet'>

type OnEvent = Event -> unit

let encode : Encode<Packet'> =
    fun pkt ->
        Dap.WebSocket.Internal.Text.encode Encoding.UTF8 <| Packet.encode pkt

let decode : Decode<Packet'> =
    fun (buffer, index, count) ->
        Packet.decode <| Dap.WebSocket.Internal.Text.decode Encoding.UTF8 (buffer, index, count)

let getSpec (logTraffic : bool) (bufferSize : int option) =
    fun () ->
        {
            LogTraffic = logTraffic
            SendType = WebSocketMessageType.Text
            BufferSize = defaultArg bufferSize DefaultBufferSize
            Encode = encode
            Decode = decode
            Event' = new Event<Evt<Packet'>>()
        }
    |> Dap.WebSocket.Conn.Logic.getSpec

let getSpawner env logTraffic bufferSize =
    getSpec logTraffic bufferSize
    |> Agent.getSpawner env
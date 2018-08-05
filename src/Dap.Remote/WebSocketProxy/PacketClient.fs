[<RequireQualifiedAccess>]
module Dap.Remote.WebSocketProxy.PacketClient

open System.Text
open System.Net.WebSockets

open Dap.Prelude
open Dap.Platform
open Dap.Remote
open Dap.Remote.Internal
open Dap.WebSocket
open Dap.WebSocket.Client.Types

[<Literal>]
let Kind = "WebSocketPacketClient"

type Agent = IAgent<Req<Packet>, Evt<Packet>>

type Evt = Evt<Packet>

let encode = Dap.Remote.WebSocketService.PacketConn.encode
let decode = Dap.Remote.WebSocketService.PacketConn.decode

let spec logTraffic bufferSize =
    Dap.WebSocket.Client.Logic.spec WebSocketMessageType.Text encode decode logTraffic bufferSize

let registerAsync' kind logTraffic bufferSize env =
    let spec = spec logTraffic bufferSize
    env |> Env.registerAsync spec kind

let registerAsync a b = registerAsync' Kind a b

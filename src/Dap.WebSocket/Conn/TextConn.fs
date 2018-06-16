[<RequireQualifiedAccess>]
module Dap.WebSocket.Conn.TextConn

open System
open System.Text
open System.Net.WebSockets

open Dap.Prelude
open Dap.Platform
open Dap.WebSocket
open Dap.WebSocket.Conn.Types

[<Literal>]
let Kind = "WebSocketTextConn"

type Agent = IAgent<Model<string>, Req<string>, Evt<string>>

type Event = Evt<string>

type OnEvent = Event -> unit

let getSpec (encoding: Encoding) (logTraffic : bool) (bufferSize : int option) =
    fun () ->
        {
            LogTraffic = logTraffic
            SendType = WebSocketMessageType.Text
            BufferSize = defaultArg bufferSize DefaultBufferSize
            Encode = Dap.WebSocket.Internal.Text.encode encoding
            Decode = Dap.WebSocket.Internal.Text.decode encoding
            Event' = new Event<Evt<string>>()
        }
    |> Logic.getSpec

let getSpawner env encoding logTraffic bufferSize =
    getSpec encoding logTraffic bufferSize
    |> Agent.getSpawner env

let getUtf8Spawner env =
    getSpawner env Encoding.UTF8

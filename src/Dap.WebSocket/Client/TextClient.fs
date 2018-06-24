[<RequireQualifiedAccess>]
module Dap.WebSocket.Client.TextClient

open System
open System.Text
open System.Net.WebSockets

open Dap.Platform
open Dap.WebSocket
open Dap.WebSocket.Client.Types

[<Literal>]
let Kind = "WebSocketTextClient"

type Agent = Agent<string>

type Event = Evt<string>

type OnEvent = Event -> unit

let getSpec (encoding: Encoding) (logTraffic : bool) (bufferSize : int option) =
    fun owner ->
        {
            LogTraffic = logTraffic
            SendType = WebSocketMessageType.Text
            BufferSize = defaultArg bufferSize DefaultBufferSize
            Encode = Dap.WebSocket.Internal.Text.encode encoding
            Decode = Dap.WebSocket.Internal.Text.decode encoding
            Event' = new Bus<Evt<string>> (owner)
        }
    |> Logic.getSpec

let getSpawner env encoding logTraffic bufferSize =
    getSpec encoding logTraffic bufferSize
    |> Agent.getSpawner env

let getUtf8Spawner env =
    getSpawner env Encoding.UTF8

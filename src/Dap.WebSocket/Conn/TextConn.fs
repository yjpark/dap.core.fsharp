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

type Agent = Agent<string>

type Event = Evt<string>

type OnEvent = Event -> unit

let getSpawner env encoding logTraffic bufferSize =
    let encode = Dap.WebSocket.Internal.Text.encode encoding
    let decode = Dap.WebSocket.Internal.Text.decode encoding
    Logic.getSpec encode decode logTraffic bufferSize
    |> Agent.getSpawner env

let getUtf8Spawner env =
    getSpawner env Encoding.UTF8

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

type Agent =  IAgent<Req<string>, Evt<string>>

type Evt = Evt<string>

let getSpawner env encoding logTraffic bufferSize =
    let encode = Dap.WebSocket.Internal.Text.encode encoding
    let decode = Dap.WebSocket.Internal.Text.decode encoding
    Logic.getSpec WebSocketMessageType.Text encode decode logTraffic bufferSize
    |> Agent.getSpawner env

let getUtf8Spawner env =
    getSpawner env Encoding.UTF8

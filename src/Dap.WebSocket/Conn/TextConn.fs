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

type Agent =  IAgent<Req<string>, Evt<string>>

type Evt = Evt<string>

let spec encoding logTraffic bufferSize =
    let encode = Dap.WebSocket.Internal.Text.encode encoding
    let decode = Dap.WebSocket.Internal.Text.decode encoding
    Logic.spec WebSocketMessageType.Text encode decode logTraffic bufferSize

let registerAsync' kind encoding logTraffic bufferSize env =
    let spec = spec encoding logTraffic bufferSize
    env |> Env.registerAsync spec kind

let registerAsync a b c = registerAsync' Kind a b c

let registerUtf8Async a b c = registerAsync Encoding.UTF8 a b c

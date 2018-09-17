[<RequireQualifiedAccess>]
module Dap.WebSocket.Client.TextClient

open System
open System.Text
open System.Net.WebSockets

open Dap.Platform
open Dap.WebSocket
module BaseTypes = Dap.WebSocket.Types
module ClientTypes = Dap.WebSocket.Client.Types

[<Literal>]
let Utf8Kind = "Utf8TextClient"

type Req = ClientTypes.Req<string>
type Evt = BaseTypes.Evt<string>
type Args = BaseTypes.Args<ClientWebSocket, string, Req>
type Agent = ClientTypes.Agent<string>

let args encoding logTraffic bufferSize =
    let encode = Dap.WebSocket.Internal.Text.encode encoding
    let decode = Dap.WebSocket.Internal.Text.decode encoding
    Args.Create logTraffic WebSocketMessageType.Text bufferSize encode decode Dap.WebSocket.Client.Logic.handleReq

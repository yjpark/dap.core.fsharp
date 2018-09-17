[<RequireQualifiedAccess>]
module Dap.WebSocket.Conn.TextConn

open System
open System.Text
open System.Net.WebSockets

open Dap.Prelude
open Dap.Platform
open Dap.WebSocket
module BaseTypes = Dap.WebSocket.Types
module ConnTypes = Dap.WebSocket.Conn.Types

[<Literal>]
let Utf8Kind = "Utf8TextConn"

type Req = ConnTypes.Req<string>
type Evt = BaseTypes.Evt<string>
type Args = BaseTypes.Args<WebSocket, string, Req>
type Agent = ConnTypes.Agent<string>

let args encoding logTraffic bufferSize =
    let encode = Dap.WebSocket.Internal.Text.encode encoding
    let decode = Dap.WebSocket.Internal.Text.decode encoding
    Args.Create logTraffic WebSocketMessageType.Text bufferSize encode decode Dap.WebSocket.Conn.Logic.handleReq
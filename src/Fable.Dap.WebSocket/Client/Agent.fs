[<RequireQualifiedAccess>]
module Dap.WebSocket.Client.Agent

open Dap.Prelude
open Dap.Platform
open Dap.WebSocket.Client.Types

[<Literal>]
let Kind = "WebSocketClient"

type Agent<'pkt> = IAgent<Req<'pkt>, Evt<'pkt>>
type Args<'pkt> = Dap.WebSocket.Client.Types.Args<'pkt>

let spawn'<'pkt> kind key args env =
    let spec = Logic.spec<'pkt> args
    env |> Env.spawn spec kind key :?> Agent<'pkt>

let spawn<'pkt> key = spawn'<'pkt> Kind key

let encodeText : Encode<string> =
    box

let decodeText : Decode<string> =
    string

let spawnText' kind key uri logTraffic =
    let args = Args<'pkt>.Create encodeText decodeText uri logTraffic
    spawn'<string> kind key args

let spawnText key = spawnText' Kind key

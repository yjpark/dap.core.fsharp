[<RequireQualifiedAccess>]
module Dap.WebSocket.Client.TextClient

open Dap.Prelude
open Dap.Platform
module ClientTypes = Dap.WebSocket.Client.Types

[<Literal>]
let Kind = "TextClient"

type Args = ClientTypes.Args<string>
type Agent = ClientTypes.Agent<string>

let encodeText : ClientTypes.Encode<string> =
    box

let decodeText : ClientTypes.Decode<string> =
    string

let args uri logTraffic =
    Args.Create encodeText decodeText uri logTraffic

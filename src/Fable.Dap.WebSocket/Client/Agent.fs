[<RequireQualifiedAccess>]
module Dap.WebSocket.Client.Actor

open Dap.Prelude
open Dap.Platform
open Dap.WebSocket.Client.Types

let create (encode : Encode<'pkt>) (decode : Decode<'pkt>)
            (key : Key) (uri : string) (logTraffic : bool) : Agent<'pkt> =
    fun _agent ->
        {
            Uri = uri
            LogTraffic = logTraffic
            Encode = encode
            Decode = decode
        }
    |> Logic.getSpec
    |> Agent.create Kind key

let encodeText : Encode<string> =
    box

let decodeText : Decode<string> =
    string

let createText =
    create encodeText decodeText
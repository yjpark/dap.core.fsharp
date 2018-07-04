[<RequireQualifiedAccess>]
module Dap.WebSocket.Client.Actor

open Dap.Prelude
open Dap.Platform
open Dap.WebSocket.Client.Types

type Agent<'pkt> = IAgent<Req<'pkt>, Evt<'pkt>>

let create'<'pkt> (encode : Encode<'pkt>) (decode : Decode<'pkt>)
            (key : Key) (uri : string) (logTraffic : bool) : Dap.WebSocket.Client.Types.Agent<'pkt> =
    fun _agent ->
        {
            Uri = uri
            LogTraffic = logTraffic
            Encode = encode
            Decode = decode
        }
    |> Logic.getSpec
    |> Agent.create Kind key

let create<'pkt> (encode : Encode<'pkt>) (decode : Decode<'pkt>)
            (key : Key) (uri : string) (logTraffic : bool) : Agent<'pkt> =
    create' encode decode key uri logTraffic
    :> Agent<'pkt>

let encodeText : Encode<string> =
    box

let decodeText : Decode<string> =
    string

let createText =
    create<string> encodeText decodeText
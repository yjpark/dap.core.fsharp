[<RequireQualifiedAccess>]
module Dap.WebSocket.Client.Actor

open Dap.Prelude
open Dap.Platform
open Dap.WebSocket.Client.Types
let create (encode : Encode<'pkt>) (decode : Decode<'pkt>)
            (key : Key) (uri : string) (logTraffic : bool) : Actor<'pkt> =
    fun () ->
        {
            Uri = uri
            LogTraffic = logTraffic
            Encode = encode
            Decode = decode
            Event' = new Event<Evt<'pkt>>()
        }
    |> Logic.getSpec
    |> Actor.create Kind key

let encodeText : Encode<string> =
    box

let decodeText : Decode<string> =
    string

let createText =
    create encodeText decodeText
[<RequireQualifiedAccess>]
module Dap.WebSocket.Client.Actor

open Dap.Prelude
open Dap.Platform
open Dap.WebSocket.Client.Types

let create (encode : Encode<'pkt>) (decode : Decode<'pkt>)
            (key : Key) (uri : string) (logTraffic : bool) : Agent<'pkt> =
    fun owner ->
        {
            Uri = uri
            LogTraffic = logTraffic
            Encode = encode
            Decode = decode
            Event' = new Bus<Evt<'pkt>> (owner)
        }
    |> Logic.getSpec
    |> Agent.create Kind key

let encodeText : Encode<string> =
    box

let decodeText : Decode<string> =
    string

let createText =
    create encodeText decodeText
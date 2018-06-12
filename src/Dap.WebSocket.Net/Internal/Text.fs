[<RequireQualifiedAccess>]
module Dap.WebSocket.Internal.Text

open System
open System.Text
open Dap.WebSocket.Types

let encode (encoding : Encoding) : Encode<string> =
    fun pkt ->
        let bytes = encoding.GetBytes(pkt)
        ArraySegment<byte>(bytes)

let decode (encoding : Encoding) : Decode<string> =
    fun (buffer, index, count) ->
        encoding.GetString(buffer, index, count)


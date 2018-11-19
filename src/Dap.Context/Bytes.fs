[<AutoOpen>]
[<RequireQualifiedAccess>]
module Dap.Context.Bytes

open System
open System.Text

#if FABLE_COMPILER
module TE = Thoth.Json.Encode
module TD = Thoth.Json.Decode
#else
open Newtonsoft.Json.Linq
module TE = Thoth.Json.Net.Encode
module TD = Thoth.Json.Net.Decode
#endif

open Dap.Prelude

type Bytes = byte []

[<RequireQualifiedAccess>]
type BytesFormat =
    | Base64
    | Custom of encoder : JsonEncoder<Bytes> * decoder : JsonDecoder<Bytes>
with
    member this.JsonEncoder : JsonEncoder<Bytes> =
        match this with
        | Base64 ->
            E.string << Convert.ToBase64String
        | Custom (encoder, _decoder) ->
            encoder
    member this.JsonDecoder : JsonDecoder<Bytes> =
        match this with
        | Base64 ->
            fun path token ->
                if token.IsString then
                    match this with
                    | Base64 ->
                        Ok <| Convert.FromBase64String (token.ToStringValue ())
                else
                    Error (path, TD.BadPrimitive("a string of base64 bytes", token))
        | Custom (_encoder, decoder) ->
            decoder
    member this.JsonSpec =
        FieldSpec.Create<Bytes> (this.JsonEncoder, this.JsonDecoder)

let toChunk (pos : int) (len : int) (bytes : Bytes) : Bytes option =
    let pos = if pos < 0 then 0 else pos
    if pos >= bytes.Length then
        None
    else
        let endPos =
            if len <= 0 then
                Int32.MaxValue
            else
                pos + len - 1
        let endPos = min endPos (bytes.Length - 1)
        Some bytes.[pos .. endPos]

let toChunks (size : int) (bytes : Bytes) : Bytes list =
    if size <= 0 then
        failWith "Invalid_Size" size
    elif bytes.Length = 0 then
        []
    else
        let num = bytes.Length / size
        let num =
            if (bytes.Length % size) > 0 then
                num + 1
            else
                num
        [0 .. num - 1]
        |> List.map (fun index ->
            let pos = index * size
            toChunk pos size bytes
            |> Option.get
        )
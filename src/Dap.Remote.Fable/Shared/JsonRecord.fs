[<AutoOpen>]
module Dap.Remote.JsonRecord

#if FABLE_COMPILER
module E = Thoth.Json.Encode
module D = Thoth.Json.Decode
type Value = E.Value
#else
open Newtonsoft.Json
open Newtonsoft.Json.Linq
module E = Thoth.Json.Net.Encode
module D = Thoth.Json.Net.Decode
type Value = JToken
#endif

open Dap.Platform

type ``JsonRecord`` =
    abstract ToJsonObject : unit -> Value

[<AutoOpen>]
module Helper =
    type JsonRecord with
        member this.ToJsonString (indent : int) =
            E.encode indent <| this.ToJsonObject ()

type NoExtra = NoExtra
with
    interface JsonRecord with
        member this.ToJsonObject() = E.nil

let decodeNoExtra : D.Decoder<NoExtra> =
    fun _token ->
        Ok NoExtra

#if FABLE_COMPILER
#else
let decodeJson (pkt : string) =
    Newtonsoft.Json.Linq.JValue.Parse pkt

let raiseDecodeJsonException (err : string) =
    failwith err

let returnJson (wrapper : 'a -> 'b) (result : Result<'a, string>) : 'b =
    match result with
    | Ok v ->
        wrapper v
    | Error err ->
        raiseDecodeJsonException err

let decodeLong : D.Decoder<int64> =
    fun token ->
        if token.Type <> JTokenType.Integer then
            Error <| D.BadPrimitive("a long", token)
        else
            try
                let value = token.Value<int64>()
                Ok value
            with _ ->
                Error <| D.BadPrimitiveExtra ("a long", token, "Value was either too large or too small for an long")

let encodeLong (value : int64) : JToken =
    JValue(value) :> JToken

let encodeInstant (instant : Instant) =
    E.string <| instantToText instant

let decodeInstant : D.Decoder<Instant> =
    fun token ->
        if token.Type <> JTokenType.String then
            Error <| D.BadPrimitive("a string of Instant", token)
        else
            instantOfText (token.Value<string> ())
            |> Result.mapError (fun e -> 
                D.BadPrimitiveExtra ("a string of Instant", token, e.Message)
            )
#endif

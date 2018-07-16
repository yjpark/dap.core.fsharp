[<AutoOpen>]
module Dap.Remote.JsonRecord

open System
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
open Dap.Prelude

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

let private TIMESTAMP_FORMAT = "yyyy-MM-ddTHH:mm:ss";

let encodeDateTime (time : DateTime) =
    E.string <| time.ToString TIMESTAMP_FORMAT

#if FABLE_COMPILER
let decodeDateTime : D.Decoder<DateTime> =
    //NOT Tested Yet
    D.string
    |> D.map (fun s ->
        //Fable doesn't support ParseExact
        DateTime.Parse (s)
    )

let encodeInstant (instant : Instant) = encodeDateTime instant
let decodeInstant = decodeDateTime

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

let decodeDateTime : D.Decoder<DateTime> =
    fun token ->
        if token.Type = JTokenType.Date then
            Ok <| token.Value<DateTime> ()
        elif token.Type = JTokenType.String then
            let s = token.Value<string> ()
            try
                DateTime.ParseExact (s, TIMESTAMP_FORMAT, System.Globalization.CultureInfo.InvariantCulture)
                |> Ok
            with e ->
                Error <| D.FailMessage ^<| sprintf "parse failed: %s -> %s" s e.Message
        else
            Error <| D.BadPrimitive ("a string", token)

#endif

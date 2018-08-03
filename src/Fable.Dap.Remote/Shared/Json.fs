[<AutoOpen>]
module Dap.Remote.Json

open System
#if FABLE_COMPILER
open Fable.Core
open Fable.Core.JsInterop

module E = Thoth.Json.Encode
module D = Thoth.Json.Decode
type Json = E.Value
#else
open Newtonsoft.Json
open Newtonsoft.Json.Linq
module E = Thoth.Json.Net.Encode
module D = Thoth.Json.Net.Decode
type Json = JToken
#endif

open Dap.Platform
open Dap.Prelude

type JsonDecoder<'json> = D.Decoder<'json>
type JsonEncoder<'json> = 'json -> Json

type IJson =
    abstract ToJson : unit -> Json

[<AutoOpen>]
module Extensions =
    type IJson with
        static member JsonEncoder : JsonEncoder<IJson> =
            fun this -> this.ToJson ()
        member this.EncodeJson (indent : int) =
            E.encode indent <| this.ToJson ()

type Boolean with
    static member JsonDecoder : JsonDecoder<Boolean> = D.bool
    static member JsonEncoder : JsonEncoder<Boolean> = E.bool
    member this.ToJson () = E.bool this

type Int32 with
    static member JsonDecoder : JsonDecoder<Int32> = D.int
    static member JsonEncoder : JsonEncoder<Int32> = E.int
    member this.ToJson () = E.int this

type String with
    static member JsonDecoder : JsonDecoder<String> = D.string
    static member JsonEncoder : JsonEncoder<String> = E.string
    member this.ToJson () = E.string this

#if FABLE_COMPILER
[<Import("identity", "../Native/Util.js")>]
let internal fableObjToJson : obj -> E.Value = jsNative

type E.Value with
    static member JsonDecoder : JsonDecoder<E.Value> =
        fun v ->
            Ok <| fableObjToJson v
    static member JsonEncoder : JsonEncoder<E.Value> = id
#else
type JToken with
    static member JsonDecoder : JsonDecoder<JToken> = D.value
    static member JsonEncoder : JsonEncoder<JToken> = id
#endif
    member this.EncodeJson (indent : int) =
        E.encode indent this

type DateTime with
    static member JsonDecoder : JsonDecoder<DateTime> =
#if FABLE_COMPILER
        //NOT Tested Yet
        D.string
        |> D.map (fun s ->
            dateTimeOfText s
            |> Result.get
        )
#else
        fun token ->
            if token.Type = JTokenType.Date then
                Ok <| token.Value<DateTime> ()
            elif token.Type = JTokenType.String then
                let s = token.Value<string> ()
                dateTimeOfText s
                |> Result.mapError (fun e ->
                    D.FailMessage ^<| sprintf "parse failed: %s -> %s" s e.Message
                )
            else
                Error <| D.BadPrimitive ("a string", token)
#endif
    static member JsonEncoder : JsonEncoder<DateTime> =
        fun this -> this.ToJson ()
    member this.ToJson () =
        E.string <| dateTimeToText this

let tryDecodeJson (decoder : JsonDecoder<'res>) pkt =
    D.decodeString decoder pkt

let decodeJson (decoder : JsonDecoder<'res>) pkt =
    tryDecodeJson decoder pkt
    |> Result.get

let tryCastJson (decoder : JsonDecoder<'res>) json =
    D.decodeValue decoder json

let castJson (decoder : JsonDecoder<'res>) json =
    tryCastJson decoder json
    |> Result.get

let returnJson (wrapper : 'a -> 'b) (result : Result<'a, string>) : 'b =
    match result with
    | Ok v ->
        wrapper v
    | Error err ->
        failwith err

#if FABLE_COMPILER
[<Import("parseJson", "../Native/Util.js")>]
let parseJson : string -> E.Value = jsNative

#else
let parseJson (pkt : string) =
    Newtonsoft.Json.Linq.JValue.Parse pkt

type Int64 with
    static member JsonDecoder : JsonDecoder<int64> =
        fun token ->
            if token.Type <> JTokenType.Integer then
                Error <| D.BadPrimitive("a long", token)
            else
                try
                    let value = token.Value<int64>()
                    Ok value
                with _ ->
                    Error <| D.BadPrimitiveExtra ("a long", token, "Value was either too large or too small for an long")
    static member JsonEncoder : JsonEncoder<Int64> =
        fun this -> this.ToJson ()
    member this.ToJson () =
        JValue(this) :> JToken

type NodaTime.Instant with
    static member JsonDecoder : JsonDecoder<Instant> =
        fun token ->
            if token.Type <> JTokenType.String then
                Error <| D.BadPrimitive("a string of Instant", token)
            else
                instantOfText (token.Value<string> ())
                |> Result.mapError (fun e ->
                    D.BadPrimitiveExtra ("a string of Instant", token, e.Message)
                )
    static member JsonEncoder : JsonEncoder<NodaTime.Instant> =
        fun this -> this.ToJson ()
    member this.ToJson () =
        E.string <| instantToText this
#endif

type E = Encoder with
    static member json (json : 'json when 'json :> IJson) = IJson.JsonEncoder (json :> IJson)
    static member json (json : Json) = Json.JsonEncoder json
    static member dateTime = DateTime.JsonEncoder
#if !FABLE_COMPILER
    static member instant = Instant.JsonEncoder

    static member long = Int64.JsonEncoder
#endif

type D = Decoder with
    static member bind (binder : 'a -> Result<'b, string>) (d1 : JsonDecoder<'a>) : JsonDecoder<'b> =
        d1
        |> D.map (fun a ->
            match binder a with
            | Ok b -> b
            | Error err -> failwith err
        )
    static member json = Json.JsonDecoder
    static member dateTime = DateTime.JsonDecoder
#if !FABLE_COMPILER
    static member instant = Instant.JsonDecoder

    static member long = Int64.JsonDecoder
#endif

[<AutoOpen>]
module Dap.Remote.Json

open System
open Microsoft.FSharp.Reflection

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

open Dap.Prelude
open Dap.Platform

type JsonDecoder<'json> = D.Decoder<'json>
type JsonEncoder<'json> = 'json -> Json

type IJson =
    abstract ToJson : unit -> Json

[<AutoOpen>]
module Extensions =
    type IJson with
        member this.EncodeJson (indent : int) =
            E.encode indent <| this.ToJson ()

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
[<Import("identity", "../Native/Util.js")>]
let fableObjToJson : obj -> E.Value = jsNative

[<Import("parseJson", "../Native/Util.js")>]
let parseJson : string -> E.Value = jsNative
#else
let parseJson (pkt : string) =
    Newtonsoft.Json.Linq.JValue.Parse pkt
#endif

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
type E.Value with
    static member JsonDecoder : JsonDecoder<E.Value> =
        fun v ->
            Ok <| fableObjToJson v
    static member JsonEncoder : JsonEncoder<E.Value> = id
    member this.IsBool = D.Helpers.isBoolean this
    member this.IsInt = D.Helpers.isNumber this && D.Helpers.isValidIntRange this
    member this.IsString = D.Helpers.isString this
    member this.IsObject = D.Helpers.isObject this
    member this.IsArray = D.Helpers.isArray this
    // Fable Only
    member this.IsNumber = D.Helpers.isNumber this
    member this.IsNaN = D.Helpers.isNaN this
    member this.IsDefined = D.Helpers.isDefined this
    member this.IsFunction = D.Helpers.isFunction this
    member this.ObjectKeys = D.Helpers.objectKeys this
#else
type JToken with
    static member JsonDecoder : JsonDecoder<JToken> = D.value
    static member JsonEncoder : JsonEncoder<JToken> = id
    member this.IsBool = (this.Type = JTokenType.Boolean)
    member this.IsInt = (this.Type = JTokenType.Integer)
    member this.IsFloat = (this.Type = JTokenType.Float)
    member this.IsString = (this.Type = JTokenType.String)
    member this.IsObject = (this.Type = JTokenType.Object)
    member this.IsArray = (this.Type = JTokenType.Array)
    // DotNet Only
    member this.IsNull = (this.Type = JTokenType.Null)
    member this.IsDate = (this.Type = JTokenType.Date)
    member this.IsTimeSpan = (this.Type = JTokenType.TimeSpan)
    member this.ToArray () =
        if this.Type = JTokenType.Array then
            this.Value<JArray>().Values()
        else
            Seq.empty
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
        fun this -> E.string <| dateTimeToText this
    member this.ToJson () = DateTime.JsonEncoder this

type Decimal with
    static member JsonDecoder : JsonDecoder<Decimal> =
        D.string |> D.map (fun s -> Decimal.Parse s)
    static member JsonEncoder : JsonEncoder<Decimal> =
        fun this -> E.string <| this.ToString ()

#if !FABLE_COMPILER
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

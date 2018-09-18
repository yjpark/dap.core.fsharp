[<AutoOpen>]
module Dap.Context.Json

open System
open Microsoft.FSharp.Reflection

#if FABLE_COMPILER
open Fable.Core
open Fable.Core.JsInterop

module E = Thoth.Json.Encode
module D = Thoth.Json.Decode
type Json = E.Value
#else
open Newtonsoft.Json.Linq
module E = Thoth.Json.Net.Encode
module D = Thoth.Json.Net.Decode
type Json = JToken
#endif

open Dap.Prelude

type JsonDecoder<'json> = D.Decoder<'json>
type JsonEncoder<'json> = 'json -> Json

type IJson =
    abstract ToJson : unit -> Json

[<AutoOpen>]
module Extensions =
    type IJson with
        member this.EncodeJson (indent : int) =
            E.encode indent <| this.ToJson ()

let tryDecodeJson (decoder : JsonDecoder<'res>) (pkt : string) =
    D.decodeString decoder pkt

let decodeJson (decoder : JsonDecoder<'res>) (pkt : string) =
    tryDecodeJson decoder pkt
    |> Result.get

let tryCastJson (decoder : JsonDecoder<'res>) (json : Json) =
    D.decodeValue decoder json

let castJson (decoder : JsonDecoder<'res>) (json : Json) =
    tryCastJson decoder json
    |> Result.get

let returnJson (wrapper : 'a -> 'b) (result : Result<'a, string>) : 'b =
    match result with
    | Ok v ->
        wrapper v
    | Error err ->
        failwith err

// Currently single json value is no considered as valid json text anymore
// Actually Newtonsoft works fine, though not sure about the fable
// So use some wrapping here to decode the properly
// https://stackoverflow.com/questions/13318420/is-a-single-string-value-considered-valid-json
let tryDecodeJsonValue (decoder : JsonDecoder<'res>) (pkt : string) =
    if pkt.StartsWith ("{") || pkt.StartsWith ("[") then
        tryDecodeJson decoder pkt
    else
        let decoder = (D.field "v" decoder)
        tryDecodeJson decoder ("{\"v\":" + pkt + "}")

let decodeJsonValue (decoder : JsonDecoder<'res>) (pkt : string) =
    tryDecodeJsonValue decoder pkt
    |> Result.get

let tryDecodeJsonString (decoder : JsonDecoder<'res>) (pkt : string) =
    if pkt.StartsWith ("\"") then
        pkt
    else
        "\"" + pkt + "\""
    |> tryDecodeJsonValue decoder

let decodeJsonString (decoder : JsonDecoder<'res>) (pkt : string) =
    tryDecodeJsonString decoder pkt
    |> Result.get

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
    member this.ToArrayValue () =
        if this.IsArray then
            this.Value<JArray>().Values()
        else
            Seq.empty
    member this.ToStringValue () =
        if this.IsString then
            //If use this.Value<string>(), some time got exception
            //"Cannot access child value on Newtonsoft.Json.Linq.JValue."
            this.ToString ()
        else
            ""
#endif
    member this.EncodeJson (indent : int) =
        E.encode indent this

type Double with
    static member JsonDecoder : JsonDecoder<float> = D.float
    static member JsonEncoder : JsonEncoder<float> = E.float
    member this.ToJson () = E.float this

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

#endif

type LogLevel with
    static member JsonEncoder : JsonEncoder<LogLevel> =
        function
        | LogLevelFatal -> "fatal"
        | LogLevelError -> "error"
        | LogLevelWarning -> "warning"
        | LogLevelInformation -> "information"
        | LogLevelDebug -> "debug"
        | LogLevelVerbose -> "verbose"
        >> E.string
    static member JsonDecoder : JsonDecoder<LogLevel> =
        D.string
        |> D.map (fun level' ->
            let level = level'.ToLower ()
            if level = "fatal" then
                LogLevelFatal
            elif level = "error" then
                LogLevelError
            elif level = "warning" then
                LogLevelWarning
            elif level = "information" then
                LogLevelInformation
            elif level = "debug" then
                LogLevelDebug
            elif level = "verbose" then
                LogLevelVerbose
            else
                failWith "Invalid_LogLevel" level'
        )

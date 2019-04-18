[<AutoOpen>]
module Dap.Context.Json

open System
open Microsoft.FSharp.Reflection

#if FABLE_COMPILER
open Fable.Core
open Fable.Core.JsInterop

module TE = Thoth.Json.Encode
module TD = Thoth.Json.Decode

type Json = Thoth.Json.JsonValue
type JsonDecoder<'json> = Thoth.Json.Decoder<'json>
type JsonDecoderError = Thoth.Json.DecoderError
type JsonErrorReason = Thoth.Json.ErrorReason
#else
open Newtonsoft.Json.Linq
module TE = Thoth.Json.Net.Encode
module TD = Thoth.Json.Net.Decode

type Json = JToken
type JsonDecoder<'json> = Thoth.Json.Net.Decoder<'json>
type JsonDecoderError = Thoth.Json.Net.DecoderError
type JsonErrorReason = Thoth.Json.Net.ErrorReason
#endif

open Dap.Prelude

type JsonEncoder<'json> = 'json -> Json

type IJson =
    abstract ToJson : unit -> Json

let toJson (json : IJson) = json.ToJson ()

let encodeJson (indent : int) (json : IJson) =
    TE.toString indent <| json.ToJson ()

[<AutoOpen>]
module Extensions =
    type IJson with
        member this.EncodeJson (indent : int) =
            TE.toString indent <| this.ToJson ()

let tryDecodeJson (decoder : JsonDecoder<'res>) (pkt : string) : Result<'res, string> =
    TD.fromString decoder pkt

let decodeJson (decoder : JsonDecoder<'res>) (pkt : string) : 'res =
    tryDecodeJson decoder pkt
    |> Result.get

let tryCastJson (decoder : JsonDecoder<'res>) (json : Json) : Result<'res, string> =
    TD.fromValue "" decoder json

let castJson (decoder : JsonDecoder<'res>) (json : Json) : 'res =
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
let tryDecodeJsonValue (decoder : JsonDecoder<'res>) (pkt : string) : Result<'res, string> =
    if pkt.StartsWith ("{") || pkt.StartsWith ("[") then
        tryDecodeJson decoder pkt
    else
        let decoder = (TD.field "v" decoder)
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
[<Import("parseJson", "../Native/Util.js")>]
let parseJson : string -> Json = jsNative
#else

let parseJson (pkt : string) =
    Newtonsoft.Json.Linq.JValue.Parse pkt
#endif

type Boolean with
    static member JsonDecoder : JsonDecoder<Boolean> = TD.bool
    static member JsonEncoder : JsonEncoder<Boolean> = TE.bool
    member this.ToJson () = TE.bool this

type Int32 with
    static member JsonDecoder : JsonDecoder<Int32> = TD.int
    static member JsonEncoder : JsonEncoder<Int32> = TE.int
    member this.ToJson () = TE.int this

type String with
    static member JsonDecoder : JsonDecoder<String> = TD.string
    static member JsonEncoder : JsonEncoder<String> = TE.string
    member this.ToJson () = TE.string this

#if FABLE_COMPILER
//Copied from Thoth, which is internal
module JsonHelpers =
    [<Emit("typeof $0")>]
    let jsTypeof (_ : obj) : string = jsNative
    [<Emit("$0 instanceof SyntaxError")>]
    let isSyntaxError (_ : obj) : bool = jsNative
    let inline isString (o: obj) : bool = o :? string
    let inline isBoolean (o: obj) : bool = o :? bool
    let inline isNumber (o: obj) : bool = jsTypeof o = "number"
    let inline isArray (o: obj) : bool = JS.Array.isArray(o)
    [<Emit("Object.getPrototypeOf($0 || false) === Object.prototype")>]
    let isObject (_ : obj) : bool = jsNative
    let inline isNaN (o: obj) : bool = JS.Number.isNaN(!!o)
    let inline isNull (o: obj): bool = isNull o
    [<Emit("-2147483648 < $0 && $0 < 2147483647 && ($0 | 0) === $0")>]
    let isValidIntRange (_: obj) : bool = jsNative
    [<Emit("isFinite($0) && !($0 % 1)")>]
    let isIntFinite (_: obj) : bool = jsNative
    [<Emit("($0 !== undefined)")>]
    let isDefined (_: obj) : bool = jsNative
    [<Emit("JSON.stringify($0, null, 4) + ''")>]
    let anyToString (_: obj) : string= jsNative
    let inline isFunction (o: obj) : bool = jsTypeof o = "function"
    let inline objectKeys (o: obj) : string seq = upcast JS.Object.keys(o)
    let inline asBool (o: obj): bool = unbox o
    let inline asInt (o: obj): int = unbox o
    let inline asFloat (o: obj): float = unbox o
    let inline asString (o: obj): string = unbox o
    let inline asArray (o: obj): obj [] = unbox o

type Object with
    (*
    static member JsonDecoder : JsonDecoder<Object> = TD.value
    static member JsonEncoder : JsonEncoder<Object> = id
    *)
    member this.IsBool = JsonHelpers.isBoolean this
    member this.IsInt = JsonHelpers.isNumber this && JsonHelpers.isValidIntRange this
    member this.IsString = JsonHelpers.isString this
    member this.IsObject = JsonHelpers.isObject this
    member this.IsArray = JsonHelpers.isArray this
    // Fable Only
    member this.IsNumber = JsonHelpers.isNumber this
    member this.IsNaN = JsonHelpers.isNaN this
    member this.IsDefined = JsonHelpers.isDefined this
    member this.IsFunction = JsonHelpers.isFunction this
    member this.ToStringValue () : string = this.ToString ()
    member this.ToArrayValue () = JsonHelpers.asArray this |> Array.toSeq
    member this.ToObjectKeys () : string seq = JsonHelpers.objectKeys this
#else
type JToken with
    static member JsonDecoder : JsonDecoder<JToken> = TD.value
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
    member this.ToArrayValue () : Json seq =
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
    member this.ToObjectKeys () : string seq =
        if this.Type = JTokenType.Object then
            this.Value<JObject>().Properties ()
            |> Seq.map (fun p -> p.Name)
        else
            []
            |> List.toSeq
#endif
    member this.EncodeJson (indent : int) =
        TE.toString indent this

type Double with
    static member JsonDecoder : JsonDecoder<float> = TD.float
    static member JsonEncoder : JsonEncoder<float> = TE.float
    member this.ToJson () = TE.float this

type Decimal with
    static member JsonDecoder : JsonDecoder<Decimal> = TD.decimal
    static member JsonEncoder : JsonEncoder<Decimal> = TE.decimal

type Int64 with
    static member JsonDecoder : JsonDecoder<int64> = TD.int64
    static member JsonEncoder : JsonEncoder<Int64> = TE.int64
    member this.ToJson () = TE.int64 this

type LogLevel with
    static member JsonEncoder : JsonEncoder<LogLevel> =
        function
        | LogLevelFatal -> "fatal"
        | LogLevelError -> "error"
        | LogLevelWarning -> "warning"
        | LogLevelInformation -> "information"
        | LogLevelDebug -> "debug"
        | LogLevelVerbose -> "verbose"
        >> TE.string
    static member JsonDecoder : JsonDecoder<LogLevel> =
        TD.string
        |> TD.map (fun level' ->
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

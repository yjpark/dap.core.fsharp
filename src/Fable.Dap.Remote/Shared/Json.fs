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

open Dap.Platform
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
let fableObjToJson : obj -> E.Value = jsNative

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
        fun this -> E.string <| dateTimeToText this
    member this.ToJson () = DateTime.JsonEncoder this

type Decimal with
    static member JsonDecoder : JsonDecoder<Decimal> =
        D.string |> D.map (fun s -> Decimal.Parse s)
    static member JsonEncoder : JsonEncoder<Decimal> =
        fun this -> E.string <| this.ToString ()


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

type FieldSpec = {
    Type : Type
    Encoder : obj -> Json
    Decoder : JsonDecoder<obj>
} with
#if FABLE_COMPILER
    [<PassGenericsAttribute>]
#endif
    static member Create<'f>
                            (encoder : JsonEncoder<'f>)
                            (decoder : JsonDecoder<'f>)
                                : FieldSpec =
        let encoder' = fun (v : obj) ->
            v :?> 'f |> encoder
        let decoder' =
            decoder |> D.map (fun v -> v :> obj)
        {
            Type = typeof<'f>
            Encoder = encoder'
            Decoder = decoder'
        }
    static member GetFieldJsonKey = sprintf "_%d_"
    static member GetFieldsEncoder (fields : FieldSpec list) (head : (string * Json) list) : JsonEncoder<obj array> =
        fun (values : obj array) ->
            values
            |> Array.toList
            |> List.zip fields
            |> List.mapi (fun index (field, v) ->
                (FieldSpec.GetFieldJsonKey index, field.Encoder v)
            )|> List.append head
            |> E.object
#if FABLE_COMPILER
    static member GetFieldsDecoder (fields : FieldSpec list) (json : obj) : Result<obj array, D.DecoderError> =
        let json = fableObjToJson json
#else
    static member GetFieldsDecoder (fields : FieldSpec list) (json : Json) : Result<obj array, D.DecoderError> =
#endif
        try
            fields
            |> List.mapi (fun index field ->
                json
                |> D.decodeValue (D.field (FieldSpec.GetFieldJsonKey index) field.Decoder)
                |> Result.mapError (fun err ->
                    sprintf "Json.FieldsDecoder [%d] <%s> -> %s" index (field.Type.FullName) err
                )|> Result.get
            )|> List.toArray
            |> Ok
        with e ->
            Error <| D.FailMessage e.Message

type JsonKind = JsonKind of string
with
    static member KindJsonKey = "_K_"
    member this.Value =
        let (JsonKind v) = this
        v
    static member TryCast (json : Json) =
        tryCastJson JsonKind.JsonDecoder json
    static member Cast (json : Json) =
        castJson JsonKind.JsonDecoder json
    static member JsonEncoder' (kind : string) =
        [(JsonKind.KindJsonKey, E.string kind)]
    static member JsonEncoder (this : JsonKind) =
        this.Value |> JsonKind.JsonEncoder' |> E.object
    static member JsonDecoder =
        D.field JsonKind.KindJsonKey D.string
        |> D.map (fun v -> JsonKind v)
    interface IJson with
        member this.ToJson () = JsonKind.JsonEncoder this

type CaseSpec<'u> = {
    Case : UnionCaseInfo
    Encoder : JsonEncoder<obj array>
    Decoder : JsonDecoder<obj array>
} with
#if FABLE_COMPILER
    [<PassGenericsAttribute>]
#endif
    static member Create (kind : string)
                            (fields : FieldSpec list)
                                : CaseSpec<'u> =
        let case = kind |> Union.findCase<'u>
        {
            Case = case
            Encoder = FieldSpec.GetFieldsEncoder fields (JsonKind.JsonEncoder' kind)
            Decoder = FieldSpec.GetFieldsDecoder fields
        }

#if FABLE_COMPILER
[<PassGenericsAttribute>]
#endif
let getUnionEncoder<'u> (spec : CaseSpec<'u> list) (v : 'u) : Json =
    let (case, values) = FSharpValue.GetUnionFields (v, typeof<'u>)
    let kind = Union.getKind<'u> v
    spec
    |> List.find (fun s -> s.Case.Name = kind)
    |> fun spec -> spec.Encoder values

#if FABLE_COMPILER
[<PassGenericsAttribute>]
#endif
let getUnionDecoder<'u> (spec : CaseSpec<'u> list) : JsonDecoder<'u> =
    fun json ->
        try
#if FABLE_COMPILER
            let json = fableObjToJson json
#endif
            let kind = JsonKind.Cast json
            spec
            |> List.find (fun s -> s.Case.Name = kind.Value)
            |> (fun spec ->
                let values =
#if FABLE_COMPILER
                    let json = json :> obj
#endif
                    json
                    |> spec.Decoder
                    |> Result.mapError (fun err ->
                        D.FailMessage <| sprintf "%s -> %A" spec.Case.Name err
                    )|> Result.get
                FSharpValue.MakeUnion(spec.Case, values) :?> 'u
                |> Ok
            )
        with err ->
            let err = sprintf "Json.UnionDecoder<%s> -> %A" (typeof<'u>.FullName) err
            Error <| D.FailMessage err

type E = Encoder with
    static member encodeJson (indent : int) (json : IJson) = E.encode indent <| json.ToJson ()
    static member json (json : IJson) = json.ToJson ()
#if FABLE_COMPILER
    [<PassGenericsAttribute>]
#endif
    static member union<'u> (spec : CaseSpec<'u> list) : JsonEncoder<'u> = getUnionEncoder spec
#if FABLE_COMPILER
    [<PassGenericsAttribute>]
#endif
    static member kind<'a> (a : 'a) =
        Union.getKind<'a> (a :> obj)
        |> JsonKind |> JsonKind.JsonEncoder
    static member dateTime = DateTime.JsonEncoder
    static member decimal = Decimal.JsonEncoder

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
#if FABLE_COMPILER
    [<PassGenericsAttribute>]
#endif
    static member union<'u> (spec : CaseSpec<'u> list) : JsonDecoder<'u> = getUnionDecoder<'u> spec

#if FABLE_COMPILER
    [<PassGenericsAttribute>]
    static member kind<'u> (json : obj) : Result<'u, D.DecoderError> =
#else
    static member kind<'u> (json : Json) : Result<'u, D.DecoderError> =
#endif
        json
        |> JsonKind.JsonDecoder
        |> Result.map (fun kind ->
            Union.fromKind<'u> kind.Value
        )
    static member dateTime = DateTime.JsonDecoder
    static member decimal = Decimal.JsonDecoder
#if !FABLE_COMPILER
    static member instant = Instant.JsonDecoder

    static member long = Int64.JsonDecoder
#endif

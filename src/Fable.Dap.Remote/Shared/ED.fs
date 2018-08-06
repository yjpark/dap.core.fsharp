[<AutoOpen>]
module Dap.Remote.ED

open System
open Microsoft.FSharp.Reflection

#if FABLE_COMPILER
open Fable.Core
module TE = Thoth.Json.Encode
module TD = Thoth.Json.Decode
#else
module TE = Thoth.Json.Net.Encode
module TD = Thoth.Json.Net.Decode
#endif

open Dap.Platform
open Dap.Prelude

type E = Encoder with
#if FABLE_COMPILER
    [<PassGenericsAttribute>]
#endif
    static member fable<'t> (v : 't) = fableToJson (v :> obj)
    static member encodeJson (indent : int) (json : IJson) = TE.encode indent <| json.ToJson ()
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
        |> TD.map (fun a ->
            match binder a with
            | Ok b -> b
            | Error err -> failwith err
        )
    static member failWith err detail =
        Error <| TD.FailMessage ^<| sprintf "%s: %A" err detail

#if FABLE_COMPILER
    [<PassGenericsAttribute>]
    static member fable<'t> (json : obj) : Result<'t, TD.DecoderError> =
        let json = fableObjToJson json
#else
    static member fable<'t> (json : Json) : Result<'t, TD.DecoderError> =
#endif
        try
            Ok <| fableCastJson<'t> json
        with e ->
            Error <| TD.FailMessage e.Message
    static member json = Json.JsonDecoder
#if FABLE_COMPILER
    [<PassGenericsAttribute>]
#endif
    static member union<'u> (spec : CaseSpec<'u> list) : JsonDecoder<'u> = getUnionDecoder<'u> spec

#if FABLE_COMPILER
    [<PassGenericsAttribute>]
    static member kind<'u> (json : obj) : Result<'u, TD.DecoderError> =
#else
    static member kind<'u> (json : Json) : Result<'u, TD.DecoderError> =
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

type E with
    static member string = TE.string
    static member int = TE.int
    static member float = TE.float
    static member nil = TE.nil
    static member bool = TE.bool
    static member object = TE.object
    static member array = TE.array
    static member list = TE.list
    static member dict = TE.dict
    static member encode = TE.encode
    static member option (encoder : JsonEncoder<'a>) = TE.option encoder

type D with
    static member unwrap (decoder : JsonDecoder<'a>) (value : Json) = TD.unwrap decoder value
    static member decodeValue (decoder : JsonDecoder<'a>) = TD.decodeValue decoder
    static member decodeString (decoder : JsonDecoder<'a>) = TD.decodeString decoder
    static member string = TD.string
    static member int = TD.int
    static member float = TD.float
    static member bool = TD.bool

    static member field (fieldName : string) (decoder : JsonDecoder<'a>) = TD.field fieldName decoder
    static member at (fieldNames : string list) (decoder : JsonDecoder<'a>) = TD.at fieldNames decoder
    static member index (requestedIndex: int) (decoder : JsonDecoder<'a>) = TD.index requestedIndex decoder
    static member list (decoder : JsonDecoder<'a>) = TD.list decoder
    static member array (decoder : JsonDecoder<'a>) = TD.array decoder
    static member keyValuePairs (decoder : JsonDecoder<'a>) = TD.keyValuePairs decoder
    static member option (decoder : JsonDecoder<'a>) = TD.option decoder
    static member oneOf (decoders : JsonDecoder<'a> list) = TD.oneOf decoders
    static member nil (output : 'a) = TD.nil output
    static member value v = TD.value v
    static member succeed (output : 'a) = TD.succeed output
    static member fail (msg : string) = TD.fail msg
    static member andThen (cb : 'a -> JsonDecoder<'b>) (decoder : JsonDecoder<'a>) = TD.andThen cb decoder
    static member map (ctor : 'a -> 'value)
        (d1 : JsonDecoder<'a>) = TD.map ctor d1
    static member map2 (ctor : 'a -> 'b -> 'value)
        (d1 : JsonDecoder<'a>)
        (d2 : JsonDecoder<'b>) = TD.map2 ctor d1 d2
    static member map3 (ctor : 'a -> 'b -> 'c -> 'value)
        (d1 : JsonDecoder<'a>)
        (d2 : JsonDecoder<'b>)
        (d3 : JsonDecoder<'c>) = TD.map3 ctor d1 d2 d3
    static member map4 (ctor : 'a -> 'b -> 'c -> 'd -> 'value)
        (d1 : JsonDecoder<'a>)
        (d2 : JsonDecoder<'b>)
        (d3 : JsonDecoder<'c>)
        (d4 : JsonDecoder<'d>) = TD.map4 ctor d1 d2 d3 d4
    static member map5 (ctor : 'a -> 'b -> 'c -> 'd -> 'e -> 'value)
        (d1 : JsonDecoder<'a>)
        (d2 : JsonDecoder<'b>)
        (d3 : JsonDecoder<'c>)
        (d4 : JsonDecoder<'d>)
        (d5 : JsonDecoder<'e>) = TD.map5 ctor d1 d2 d3 d4 d5
    static member map6 (ctor : 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'value)
        (d1 : JsonDecoder<'a>)
        (d2 : JsonDecoder<'b>)
        (d3 : JsonDecoder<'c>)
        (d4 : JsonDecoder<'d>)
        (d5 : JsonDecoder<'e>)
        (d6 : JsonDecoder<'f>) = TD.map6 ctor d1 d2 d3 d4 d5 d6
    static member map7 (ctor : 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'value)
        (d1 : JsonDecoder<'a>)
        (d2 : JsonDecoder<'b>)
        (d3 : JsonDecoder<'c>)
        (d4 : JsonDecoder<'d>)
        (d5 : JsonDecoder<'e>)
        (d6 : JsonDecoder<'f>)
        (d7 : JsonDecoder<'g>) = TD.map7 ctor d1 d2 d3 d4 d5 d6 d7
    static member map8 (ctor : 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'value)
        (d1 : JsonDecoder<'a>)
        (d2 : JsonDecoder<'b>)
        (d3 : JsonDecoder<'c>)
        (d4 : JsonDecoder<'d>)
        (d5 : JsonDecoder<'e>)
        (d6 : JsonDecoder<'f>)
        (d7 : JsonDecoder<'g>)
        (d8 : JsonDecoder<'h>) = TD.map8 ctor d1 d2 d3 d4 d5 d6 d7 d8
    static member dict (decoder : JsonDecoder<'a>) = TD.dict decoder
    static member custom d1 d2 = TD.custom d1 d2
    static member hardcoded<'a, 'b> (a : 'a) (decoder : JsonDecoder<'a -> 'b>) (json : Json) =
#if FABLE_COMPILER
        TD.hardcoded<'a, 'b, Json> a decoder json
#else
        TD.hardcoded<'a, 'b> a decoder json
#endif
    static member required (key : string) (valDecoder : JsonDecoder<'a>) (decoder : JsonDecoder<'a -> 'b>) = TD.required key valDecoder decoder
    static member requiredAt (path : string list) (valDecoder : JsonDecoder<'a>) (decoder : JsonDecoder<'a -> 'b>) = TD.requiredAt path valDecoder decoder
    static member decode output value = TD.decode output value
    static member resolve d1 = TD.resolve d1
    static member optionalDecoder pathDecoder valDecoder fallback = TD.optionalDecoder pathDecoder valDecoder fallback
    static member optional key valDecoder fallback decoder = TD.optional key valDecoder fallback decoder
    static member optionalAt path valDecoder fallback decoder = TD.optionalAt path valDecoder fallback decoder

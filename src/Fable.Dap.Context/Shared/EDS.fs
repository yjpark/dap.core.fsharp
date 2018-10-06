[<AutoOpen>]
module Dap.Context.EDS

open System
open Microsoft.FSharp.Reflection

#if FABLE_COMPILER
open Fable.Core
module TE = Thoth.Json.Encode
module TD = Thoth.Json.Decode
#else
open Newtonsoft.Json.Linq
module TE = Thoth.Json.Net.Encode
module TD = Thoth.Json.Net.Decode
#endif

open Dap.Prelude

type E = JsonEncodeHelper with
    static member encode (indent : int) (json : Json) = TE.toString indent json
    static member encodeJson (indent : int) (json : IJson) = TE.toString indent <| json.ToJson ()
    static member json (json : IJson) = json.ToJson ()
    static member union<'u> (spec : CaseSpec<'u> list
        #if FABLE_COMPILER
            , [<Inject>] ?resolver: ITypeResolver<'u>
        #endif
            ) : JsonEncoder<'u> =
    #if FABLE_COMPILER
        Union.getEncoder<'u> (spec, ?resolver=resolver)
    #else
        Union.getEncoder<'u> spec
    #endif
    static member kindStr<'u>
        #if FABLE_COMPILER
            ([<Inject>] ?resolver: ITypeResolver<'u>)
        #else
            ()
        #endif
            : JsonEncoder<'u> =
        fun v ->
        #if FABLE_COMPILER
            Union.getKind (v, ?resolver=resolver)
        #else
            Union.getKind (v)
        #endif
            |> TE.string
    static member kind<'u>
        #if FABLE_COMPILER
            ([<Inject>] ?resolver: ITypeResolver<'u>)
        #else
            ()
        #endif
            : JsonEncoder<'u> =
        fun v ->
        #if FABLE_COMPILER
            Union.getKind (v, ?resolver=resolver)
        #else
            Union.getKind (v)
        #endif
            |> JsonKind |> JsonKind.JsonEncoder
    static member long = TE.int64
    static member array (encoder : JsonEncoder<'a>) (v : 'a []) =
        v |> Array.map encoder |> TE.array
    static member list (encoder : JsonEncoder<'a>) (v : 'a list) =
        v |> List.map encoder |> TE.list
    static member dict (encoder : JsonEncoder<'a>) (v : Map<string, 'a>) =
        v |> Map.map (fun _k v -> encoder v) |> TE.dict
    static member jsonList (v : Json list) = TE.list v
    static member emptyList = TE.list []
    static member emptyObject = TE.object []
    static member jsonDict (v : Map<string, Json>) = TE.dict v
#if FABLE_COMPILER
    static member object (values : (string * Json) list) : Json =
        values
        |> List.toSeq
        |> TE.object
#else
    static member object = TE.object
#endif

type D = JsonDecodeHelper with
    static member failWith path err detail =
        Error (path, TD.FailMessage ^<| sprintf "%s: %A" err detail)
    static member json : JsonDecoder<Json> = TD.value
    static member long = TD.int64
    static member union<'u> (spec : CaseSpec<'u> list
        #if FABLE_COMPILER
            , [<Inject>] ?resolver: ITypeResolver<'u>
        #endif
            ) : JsonDecoder<'u> =
    #if FABLE_COMPILER
        Union.getDecoder<'u> (spec, ?resolver=resolver)
    #else
        Union.getDecoder<'u> spec
    #endif
    static member kindStr<'u>
        #if FABLE_COMPILER
            ([<Inject>] ?resolver: ITypeResolver<'u>)
        #else
            ()
        #endif
            : JsonDecoder<'u> =
        fun path json ->
            TD.string path json
            |> Result.map (fun kind ->
            #if FABLE_COMPILER
                Union.fromKind<'u> (kind, ?resolver=resolver)
            #else
                Union.fromKind<'u> kind
            #endif
            )
    static member kind<'u>
        #if FABLE_COMPILER
            ([<Inject>] ?resolver: ITypeResolver<'u>)
        #else
            ()
        #endif
            : JsonDecoder<'u> =
        fun path json ->
            JsonKind.JsonDecoder path json
            |> Result.map (fun kind ->
            #if FABLE_COMPILER
                Union.fromKind<'u> (kind.Value, ?resolver=resolver)
            #else
                Union.fromKind<'u> kind.Value
            #endif
            )

type E with
    static member string = TE.string
    static member guid = TE.guid
    static member int = TE.int
    static member float = TE.float
    static member decimal = TE.decimal
    static member nil = TE.nil
    static member bool = TE.bool
    static member bigint = TE.bigint
    static member int64 = TE.int64
    static member uint64 = TE.uint64
    static member datetime = TE.datetime
    static member datetimeOffset = TE.datetimeOffset
    static member tuple2
            (enc1 : JsonEncoder<'T1>)
            (enc2 : JsonEncoder<'T2>)
            (v1, v2) : Json =
        TE.tuple2 enc1 enc2 (v1, v2)
    static member tuple3
            (enc1 : JsonEncoder<'T1>)
            (enc2 : JsonEncoder<'T2>)
            (enc3 : JsonEncoder<'T3>)
            (v1, v2, v3) : Json =
        TE.tuple3 enc1 enc2 enc3 (v1, v2, v3)
    static member tuple4
            (enc1 : JsonEncoder<'T1>)
            (enc2 : JsonEncoder<'T2>)
            (enc3 : JsonEncoder<'T3>)
            (enc4 : JsonEncoder<'T4>)
            (v1, v2, v3, v4) : Json =
        TE.tuple4 enc1 enc2 enc3 enc4 (v1, v2, v3, v4)
    static member tuple5
            (enc1 : JsonEncoder<'T1>)
            (enc2 : JsonEncoder<'T2>)
            (enc3 : JsonEncoder<'T3>)
            (enc4 : JsonEncoder<'T4>)
            (enc5 : JsonEncoder<'T5>)
            (v1, v2, v3, v4, v5) : Json =
        TE.tuple5 enc1 enc2 enc3 enc4 enc5 (v1, v2, v3, v4, v5)
    static member tuple6
            (enc1 : JsonEncoder<'T1>)
            (enc2 : JsonEncoder<'T2>)
            (enc3 : JsonEncoder<'T3>)
            (enc4 : JsonEncoder<'T4>)
            (enc5 : JsonEncoder<'T5>)
            (enc6 : JsonEncoder<'T6>)
            (v1, v2, v3, v4, v5, v6) : Json =
        TE.tuple6 enc1 enc2 enc3 enc4 enc5 enc6 (v1, v2, v3, v4, v5, v6)
    static member tuple7
            (enc1 : JsonEncoder<'T1>)
            (enc2 : JsonEncoder<'T2>)
            (enc3 : JsonEncoder<'T3>)
            (enc4 : JsonEncoder<'T4>)
            (enc5 : JsonEncoder<'T5>)
            (enc6 : JsonEncoder<'T6>)
            (enc7 : JsonEncoder<'T7>)
            (v1, v2, v3, v4, v5, v6, v7) : Json =
        TE.tuple7 enc1 enc2 enc3 enc4 enc5 enc6 enc7 (v1, v2, v3, v4, v5, v6, v7)
    static member tuple8
            (enc1 : JsonEncoder<'T1>)
            (enc2 : JsonEncoder<'T2>)
            (enc3 : JsonEncoder<'T3>)
            (enc4 : JsonEncoder<'T4>)
            (enc5 : JsonEncoder<'T5>)
            (enc6 : JsonEncoder<'T6>)
            (enc7 : JsonEncoder<'T7>)
            (enc8 : JsonEncoder<'T8>)
            (v1, v2, v3, v4, v5, v6, v7, v8) : Json =
        TE.tuple8 enc1 enc2 enc3 enc4 enc5 enc6 enc7 enc8 (v1, v2, v3, v4, v5, v6, v7, v8)
    static member toString = TE.toString
    static member option (encoder : JsonEncoder<'a>) = TE.option encoder

type D with
    static member unwrap (path : string) (decoder : JsonDecoder<'a>) (value : Json) = TD.unwrap path decoder value
    static member fromValue (path : string) (decoder : JsonDecoder<'a>) = TD.fromValue path decoder
    static member fromString (decoder : JsonDecoder<'a>) = TD.fromString decoder
    static member string = TD.string
    static member guid = TD.guid
    static member int = TD.int
    static member int64 = TD.int64
    static member uint64 = TD.uint64
    static member bigint = TD.bigint
    static member bool = TD.bool
    static member float = TD.float
    static member decimal = TD.decimal
    static member datetime = TD.datetime
    static member datetimeOffset = TD.datetimeOffset
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
            (d1 : JsonDecoder<'a>) =
        TD.map ctor d1
    static member map2 (ctor : 'a -> 'b -> 'value)
            (d1 : JsonDecoder<'a>)
            (d2 : JsonDecoder<'b>) =
        TD.map2 ctor d1 d2
    static member map3 (ctor : 'a -> 'b -> 'c -> 'value)
            (d1 : JsonDecoder<'a>)
            (d2 : JsonDecoder<'b>)
            (d3 : JsonDecoder<'c>) =
        TD.map3 ctor d1 d2 d3
    static member map4 (ctor : 'a -> 'b -> 'c -> 'd -> 'value)
            (d1 : JsonDecoder<'a>)
            (d2 : JsonDecoder<'b>)
            (d3 : JsonDecoder<'c>)
            (d4 : JsonDecoder<'d>) =
        TD.map4 ctor d1 d2 d3 d4
    static member map5 (ctor : 'a -> 'b -> 'c -> 'd -> 'e -> 'value)
            (d1 : JsonDecoder<'a>)
            (d2 : JsonDecoder<'b>)
            (d3 : JsonDecoder<'c>)
            (d4 : JsonDecoder<'d>)
            (d5 : JsonDecoder<'e>) =
        TD.map5 ctor d1 d2 d3 d4 d5
    static member map6 (ctor : 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'value)
            (d1 : JsonDecoder<'a>)
            (d2 : JsonDecoder<'b>)
            (d3 : JsonDecoder<'c>)
            (d4 : JsonDecoder<'d>)
            (d5 : JsonDecoder<'e>)
            (d6 : JsonDecoder<'f>) =
        TD.map6 ctor d1 d2 d3 d4 d5 d6
    static member map7 (ctor : 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'value)
            (d1 : JsonDecoder<'a>)
            (d2 : JsonDecoder<'b>)
            (d3 : JsonDecoder<'c>)
            (d4 : JsonDecoder<'d>)
            (d5 : JsonDecoder<'e>)
            (d6 : JsonDecoder<'f>)
            (d7 : JsonDecoder<'g>) =
        TD.map7 ctor d1 d2 d3 d4 d5 d6 d7
    static member map8 (ctor : 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'value)
            (d1 : JsonDecoder<'a>)
            (d2 : JsonDecoder<'b>)
            (d3 : JsonDecoder<'c>)
            (d4 : JsonDecoder<'d>)
            (d5 : JsonDecoder<'e>)
            (d6 : JsonDecoder<'f>)
            (d7 : JsonDecoder<'g>)
            (d8 : JsonDecoder<'h>) =
        TD.map8 ctor d1 d2 d3 d4 d5 d6 d7 d8
    static member dict (decoder : JsonDecoder<'a>) = TD.dict decoder
    static member object (builder: TD.IGetters -> 'value) : JsonDecoder<'value> = TD.object builder
    static member tuple2 (decoder1: JsonDecoder<'T1>)
            (decoder2: JsonDecoder<'T2>) : JsonDecoder<'T1 * 'T2> =
        TD.tuple2 decoder1 decoder2
    static member tuple3 (decoder1: JsonDecoder<'T1>)
            (decoder2: JsonDecoder<'T2>)
            (decoder3: JsonDecoder<'T3>) : JsonDecoder<'T1 * 'T2 * 'T3> =
        TD.tuple3 decoder1 decoder2 decoder3
    static member tuple4 (decoder1: JsonDecoder<'T1>)
            (decoder2: JsonDecoder<'T2>)
            (decoder3: JsonDecoder<'T3>)
            (decoder4: JsonDecoder<'T4>) : JsonDecoder<'T1 * 'T2 * 'T3 * 'T4> =
        TD.tuple4 decoder1 decoder2 decoder3 decoder4
    static member tuple5 (decoder1: JsonDecoder<'T1>)
            (decoder2: JsonDecoder<'T2>)
            (decoder3: JsonDecoder<'T3>)
            (decoder4: JsonDecoder<'T4>)
            (decoder5: JsonDecoder<'T5>) : JsonDecoder<'T1 * 'T2 * 'T3 * 'T4 * 'T5> =
        TD.tuple5 decoder1 decoder2 decoder3 decoder4 decoder5
    static member tuple6 (decoder1: JsonDecoder<'T1>)
            (decoder2: JsonDecoder<'T2>)
            (decoder3: JsonDecoder<'T3>)
            (decoder4: JsonDecoder<'T4>)
            (decoder5: JsonDecoder<'T5>)
            (decoder6: JsonDecoder<'T6>) : JsonDecoder<'T1 * 'T2 * 'T3 * 'T4 * 'T5 * 'T6> =
        TD.tuple6 decoder1 decoder2 decoder3 decoder4 decoder5 decoder6
    static member tuple7 (decoder1: JsonDecoder<'T1>)
            (decoder2: JsonDecoder<'T2>)
            (decoder3: JsonDecoder<'T3>)
            (decoder4: JsonDecoder<'T4>)
            (decoder5: JsonDecoder<'T5>)
            (decoder6: JsonDecoder<'T6>)
            (decoder7: JsonDecoder<'T7>) : JsonDecoder<'T1 * 'T2 * 'T3 * 'T4 * 'T5 * 'T6 * 'T7> =
        TD.tuple7 decoder1 decoder2 decoder3 decoder4 decoder5 decoder6 decoder7
    static member tuple8 (decoder1: JsonDecoder<'T1>)
            (decoder2: JsonDecoder<'T2>)
            (decoder3: JsonDecoder<'T3>)
            (decoder4: JsonDecoder<'T4>)
            (decoder5: JsonDecoder<'T5>)
            (decoder6: JsonDecoder<'T6>)
            (decoder7: JsonDecoder<'T7>)
            (decoder8: JsonDecoder<'T8>) : JsonDecoder<'T1 * 'T2 * 'T3 * 'T4 * 'T5 * 'T6 * 'T7 * 'T8> =
        TD.tuple8 decoder1 decoder2 decoder3 decoder4 decoder5 decoder6 decoder7 decoder8

type TD.IRequiredGetter with
    member this.Custom (decoder : JsonDecoder<'value>) =
        this.At [] decoder
    member this.Index (requestedIndex: int) (decoder : JsonDecoder<'value>) =
        this.Custom (TD.index requestedIndex decoder)

type TD.IOptionalGetter with
    member this.Custom (decoder : JsonDecoder<'value>) =
        this.At [] decoder
    member this.Index (requestedIndex: int) (decoder : JsonDecoder<'value>) =
        this.Custom (TD.index requestedIndex decoder)

type TD.IGetters with
    member this.Custom (decoder : JsonDecoder<'value>) =
        this.Required.Custom decoder
    member this.Json : Json =
        this.Custom D.json

type S = JsonSpecHelper with
    static member option<'v>
            (
                encoder : JsonEncoder<'v>,
                decoder : JsonDecoder<'v>
            #if FABLE_COMPILER
                , [<Inject>] ?resolver: ITypeResolver<'v option>
            #endif
            ) =
    #if FABLE_COMPILER
        FieldSpec.Create<'v option> (E.option encoder, D.option decoder, ?resolver=resolver)
    #else
        FieldSpec.Create<'v option> (E.option encoder, D.option decoder)
    #endif
    static member list<'v>
            (
                encoder : JsonEncoder<'v>,
                decoder : JsonDecoder<'v>
            #if FABLE_COMPILER
                , [<Inject>] ?resolver: ITypeResolver<'v list>
            #endif
            ) =
    #if FABLE_COMPILER
        FieldSpec.Create<'v list> (E.list encoder, D.list decoder, ?resolver=resolver)
    #else
        FieldSpec.Create<'v list> (E.list encoder, D.list decoder)
    #endif
    static member json = FieldSpec.Create<Json> (id, D.value)
    static member bool = FieldSpec.Create<bool> (E.bool, D.bool)
    static member int = FieldSpec.Create<int> (E.int, D.int)
    static member long = FieldSpec.Create<int64> (E.long, D.long)
    static member string = FieldSpec.Create<string> (E.string, D.string)
    static member float = FieldSpec.Create<float> (E.float, D.float)
    static member decimal = FieldSpec.Create<decimal> (E.decimal, D.decimal)

type Boolean with
    static member JsonSpec = S.bool

type Int32 with
    static member JsonSpec = S.int

type String with
    static member JsonSpec = S.string

type Double with
    static member JsonSpec = S.float

type Decimal with
    static member JsonSpec = S.decimal

type Int64 with
    static member JsonSpec = S.long

type LogLevel with
    static member JsonSpec = FieldSpec.Create<LogLevel> (LogLevel.JsonEncoder, LogLevel.JsonDecoder)

[<AutoOpen>]
module Dap.Remote.Spec

open System
open Microsoft.FSharp.Reflection

#if FABLE_COMPILER
open Fable.Core
module E = Thoth.Json.Encode
module D = Thoth.Json.Decode
#else
module E = Thoth.Json.Net.Encode
module D = Thoth.Json.Net.Decode
#endif

open Dap.Prelude
open Dap.Platform

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

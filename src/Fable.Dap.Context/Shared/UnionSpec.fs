[<AutoOpen>]
module Dap.Context.UnionSpec

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

open Dap.Prelude

type FieldSpec = {
    Type : Type
    Encoder : JsonEncoder<obj>
    Decoder : JsonDecoder<obj>
} with
    static member Create<'f>
                (
                    encoder : JsonEncoder<'f>,
                    decoder : JsonDecoder<'f>
                #if FABLE_COMPILER
                    , [<Inject>] ?resolver: ITypeResolver<'f>
                #endif
                ) : FieldSpec =
    #if FABLE_COMPILER
        let fieldType = resolver.Value.ResolveType ()
    #else
        let fieldType = typeof<'f>
    #endif
        let encoder' = fun (v : obj) ->
            v :?> 'f |> encoder
        let decoder' =
            decoder |> TD.map (fun v -> v :> obj)
        {
            Type = fieldType
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
            |> TE.object
    static member GetFieldsDecoder (fields : FieldSpec list) (path : string) (json : Json) : Result<obj array, TD.DecoderError> =
        try
            fields
            |> List.mapi (fun index field ->
                json
                |> TD.fromValue path (TD.field (FieldSpec.GetFieldJsonKey index) field.Decoder)
                |> Result.mapError (fun err ->
                    sprintf "Json.FieldsDecoder [%d] <%s> -> %s" index (field.Type.FullName) err
                )|> Result.get
            )|> List.toArray
            |> Ok
        with e ->
            Error (path, TD.FailMessage e.Message)

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
        [(JsonKind.KindJsonKey, TE.string kind)]
    static member JsonEncoder (this : JsonKind) =
        this.Value |> JsonKind.JsonEncoder' |> TE.object
    static member JsonDecoder =
        TD.field JsonKind.KindJsonKey TD.string
        |> TD.map (fun v -> JsonKind v)
    interface IJson with
        member this.ToJson () = JsonKind.JsonEncoder this

type CaseSpec<'u> = {
    Case : UnionCaseInfo
    Encoder : JsonEncoder<obj array>
    Decoder : JsonDecoder<obj array>
} with
    static member Create (kind : string, fields : FieldSpec list
        #if FABLE_COMPILER
            , [<Inject>] ?resolver: ITypeResolver<'u>
        #endif
            ) : CaseSpec<'u> =
    #if FABLE_COMPILER
        let case = Union.findCase<'u> (kind, ?resolver=resolver)
    #else
        let case = Union.findCase<'u> kind
    #endif
        {
            Case = case
            Encoder = FieldSpec.GetFieldsEncoder fields (JsonKind.JsonEncoder' kind)
            Decoder = FieldSpec.GetFieldsDecoder fields
        }

type Union with
    static member getEncoder<'u> (spec : CaseSpec<'u> list
        #if FABLE_COMPILER
            , [<Inject>] ?resolver: ITypeResolver<'u>
        #endif
            ) : JsonEncoder<'u> =
        fun v ->
        #if FABLE_COMPILER
            let uType = resolver.Value.ResolveType ()
        #else
            let uType = typeof<'u>
        #endif
            let (case, values) = FSharpValue.GetUnionFields (v, uType)
        #if FABLE_COMPILER
            let kind = Union.getKind<'u> (v, ?resolver=resolver)
        #else
            let kind = Union.getKind<'u> v
        #endif
            spec
            |> List.find (fun s -> s.Case.Name = kind)
            |> fun spec -> spec.Encoder values
    static member  getDecoder<'u> (spec : CaseSpec<'u> list
        #if FABLE_COMPILER
            , [<Inject>] ?resolver: ITypeResolver<'u>
        #endif
            ) : JsonDecoder<'u> =
    #if FABLE_COMPILER
        let uType = resolver.Value.ResolveType ()
    #else
        let uType = typeof<'u>
    #endif
        fun path json ->
            try
                let kind = JsonKind.Cast json
                spec
                |> List.find (fun s -> s.Case.Name = kind.Value)
                |> (fun spec ->
                    let values =
                        spec.Decoder path json
                        |> Result.mapError (fun err ->
                            (path, TD.FailMessage <| sprintf "%s -> %A" spec.Case.Name err)
                        )|> Result.get
                    FSharpValue.MakeUnion(spec.Case, values) :?> 'u
                    |> Ok
                )
            with err ->
                let err = sprintf "Json.UnionDecoder<%s> -> %A" uType.FullName err
                Error (path, TD.FailMessage err)

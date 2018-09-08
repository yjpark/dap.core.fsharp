[<AutoOpen>]
module Dap.Platform.EDSMB

open System

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
open Dap.Context
open Dap.Context.Meta

type DateTime with
    static member JsonEncoder : JsonEncoder<DateTime> =
        fun this -> E.string <| dateTimeToText this
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
                    TD.FailMessage ^<| sprintf "parse failed: %s -> %s" s e.Message
                )
            else
                Error <| TD.BadPrimitive ("a string", token)
#endif
    static member JsonSpec =
        FieldSpec.Create<DateTime>
            DateTime.JsonEncoder DateTime.JsonDecoder
    member this.ToJson () = DateTime.JsonEncoder this

#if !FABLE_COMPILER
type NodaTime.Instant with
    static member JsonEncoder : JsonEncoder<NodaTime.Instant> =
        E.string << instantToText
    static member JsonDecoder : JsonDecoder<Instant> =
        fun token ->
            if token.Type = JTokenType.Date then
                Ok <| Instant.FromDateTimeUtc (token.Value<DateTime> ())
            elif token.Type <> JTokenType.String then
                Error <| TD.BadPrimitive("a string of Instant", token)
            else
                instantOfText (token.Value<string> ())
                |> Result.mapError (fun e ->
                    TD.BadPrimitiveExtra ("a string of Instant", token, e.Message)
                )
    static member JsonSpec =
        FieldSpec.Create<NodaTime.Instant>
            NodaTime.Instant.JsonEncoder NodaTime.Instant.JsonDecoder
    member this.ToJson () = NodaTime.Instant.JsonEncoder this
#endif

type E with
    static member ident = Ident.JsonEncoder
    static member dateTime = DateTime.JsonEncoder
#if !FABLE_COMPILER
    static member instant = Instant.JsonEncoder
#endif

type D with
    static member ident = Ident.JsonDecoder
    static member dateTime = DateTime.JsonDecoder
#if !FABLE_COMPILER
    static member instant = Instant.JsonDecoder
#endif

type S with
    static member ident = Ident.JsonSpec
    static member dateTime = DateTime.JsonSpec
#if !FABLE_COMPILER
    static member instant = Instant.JsonSpec
#endif

type M with
    static member ident (key, initValue, validator) =
        PropMeta.Create "Ident" "E.ident" "D.ident" "S.ident" VarProperty
            key initValue validator
    static member ident (key, initValue) =
        M.ident (key, initValue, "")
    static member ident (key) =
        M.ident (key, "noIdent")
    static member dateTime (key, initValue, validator) =
        PropMeta.Create "System.DateTime" "E.dateTime" "D.dateTime" "S.dateTime" VarProperty
            key initValue validator
    static member dateTime (key, initValue) =
        M.dateTime (key, initValue, "")
    static member dateTime (key) =
        M.dateTime (key, "System.DateTime.UtcNow")
#if !FABLE_COMPILER
    static member instant (key, initValue, validator) =
        PropMeta.Create "Instant" "E.instant" "D.instant" "S.instant" VarProperty
            key initValue validator
    static member instant (key, initValue) =
        M.instant (key, initValue, "")
    static member instant (key) =
        M.instant (key, "(getNow' ())")
#endif

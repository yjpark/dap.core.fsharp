[<AutoOpen>]
module Dap.Platform.Json

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
                    TD.FailMessage ^<| sprintf "parse failed: %s -> %s" s e.Message
                )
            else
                Error <| TD.BadPrimitive ("a string", token)
#endif
    static member JsonEncoder : JsonEncoder<DateTime> =
        fun this -> E.string <| dateTimeToText this
    member this.ToJson () = DateTime.JsonEncoder this

#if !FABLE_COMPILER
type NodaTime.Instant with
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
    static member JsonEncoder : JsonEncoder<NodaTime.Instant> =
        fun this -> this.ToJson ()
    member this.ToJson () =
        E.string <| instantToText this
#endif

type E with
    static member dateTime = DateTime.JsonEncoder
#if !FABLE_COMPILER
    static member instant = Instant.JsonEncoder
#endif

type D with
    static member dateTime = DateTime.JsonDecoder
#if !FABLE_COMPILER
    static member instant = Instant.JsonDecoder
#endif

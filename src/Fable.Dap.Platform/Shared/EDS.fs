[<AutoOpen>]
module Dap.Platform.EDS

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
    static member JsonEncoder : JsonEncoder<NodaTime.Instant> = InstantFormat.General.JsonEncoder
    static member JsonDecoder : JsonDecoder<Instant> = InstantFormat.General.JsonDecoder
    static member JsonSpec = InstantFormat.General.JsonDecoder
    member this.ToJson () = NodaTime.Instant.JsonEncoder this

type NodaTime.Duration with
    static member JsonEncoder : JsonEncoder<NodaTime.Duration> = DurationFormat.RoundTrip.JsonEncoder
    static member JsonDecoder : JsonDecoder<Duration> = DurationFormat.RoundTrip.JsonDecoder
    static member JsonSpec = DurationFormat.RoundTrip.JsonSpec
    member this.ToJson () = NodaTime.Duration.JsonEncoder this
#endif

type E with
    static member ident = Ident.JsonEncoder
    static member dateTime = DateTime.JsonEncoder
#if !FABLE_COMPILER
    static member instant = Instant.JsonEncoder
    static member duration = Duration.JsonEncoder
#endif

type D with
    static member ident = Ident.JsonDecoder
    static member dateTime = DateTime.JsonDecoder
#if !FABLE_COMPILER
    static member instant = Instant.JsonDecoder
    static member duration = Duration.JsonDecoder
#endif

type S with
    static member ident = Ident.JsonSpec
    static member dateTime = DateTime.JsonSpec
#if !FABLE_COMPILER
    static member instant = Instant.JsonSpec
    static member duration = Duration.JsonSpec
#endif
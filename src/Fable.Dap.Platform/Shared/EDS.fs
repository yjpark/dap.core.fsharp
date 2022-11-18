[<AutoOpen>]
module Dap.Platform.EDS

open System

#if FABLE_COMPILER
open Fable.Core
module TE = Thoth.Json.Encode
module TD = Thoth.Json.Decode
open Thoth.Json
#else
open Newtonsoft.Json.Linq
module TE = Thoth.Json.Net.Encode
module TD = Thoth.Json.Net.Decode
open Thoth.Json.Net
#endif

open Dap.Prelude
open Dap.Context

type DateTime with
    static member JsonEncoder : JsonEncoder<DateTime> = TE.datetime
    static member JsonDecoder : JsonDecoder<DateTime> = TD.datetimeUtc
    static member JsonSpec = FieldSpec.Create<DateTime> (DateTime.JsonEncoder, DateTime.JsonDecoder)
    member this.ToJson () = DateTime.JsonEncoder this

type TimeSpan with
    static member JsonEncoder : JsonEncoder<TimeSpan> =
        TE.timespan
    static member JsonDecoder : JsonDecoder<TimeSpan> =
        fun path json ->
            if JsonHelpers.isString json then
                let v = JsonHelpers.asString json
                match System.TimeSpan.TryParse (v) with
                | true, x ->
                    Ok x
                | false, x ->
                    try
                        let segments = v.Split ([| ':' |])
                        //TODO: Temp fix for now, seems Fable's TimeSpan.TryParse is not working properly ATM
                        if segments.Length = 4 then
                            let segments = segments |> Array.map (System.Convert.ToInt32)
                            Ok (new TimeSpan (segments.[0], segments.[1], segments.[2], segments.[3]))
                        else
                            Error (path, BadPrimitive ("a timespan", json))
                    with e ->
                        Error (path, BadPrimitiveExtra ("a timespan", json, e.Message))
            else
                Error (path, BadPrimitive("a timespan", json))
    static member JsonSpec = FieldSpec.Create<TimeSpan> (TimeSpan.JsonEncoder, TimeSpan.JsonDecoder)
    member this.ToJson () = TimeSpan.JsonEncoder this

#if !FABLE_COMPILER
type NodaTime.Instant with
    static member JsonEncoder : JsonEncoder<NodaTime.Instant> = InstantFormat.General.JsonEncoder
    static member JsonDecoder : JsonDecoder<Instant> = InstantFormat.General.JsonDecoder
    static member JsonSpec = InstantFormat.General.JsonSpec
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
    static member timeSpan = TimeSpan.JsonEncoder
#if FABLE_COMPILER
    static member instant = DateTime.JsonEncoder
    static member duration = TimeSpan.JsonEncoder
#else
    static member instant = Instant.JsonEncoder
    static member duration = Duration.JsonEncoder
#endif

type D with
    static member ident = Ident.JsonDecoder
    static member dateTime = DateTime.JsonDecoder
    static member timeSpan = TimeSpan.JsonDecoder
#if FABLE_COMPILER
    static member instant = DateTime.JsonDecoder
    static member duration = TimeSpan.JsonDecoder
#else
    static member instant = Instant.JsonDecoder
    static member duration = Duration.JsonDecoder
#endif

type S with
    static member ident = Ident.JsonSpec
    static member dateTime = DateTime.JsonSpec
    static member timeSpan = TimeSpan.JsonSpec
#if FABLE_COMPILER
    static member instant = DateTime.JsonSpec
    static member duration = TimeSpan.JsonSpec
#else
    static member instant = Instant.JsonSpec
    static member duration = Duration.JsonSpec
#endif

[<AutoOpen>]
module Dap.Platform.Clock

open System
#if FABLE_COMPILER
type Instant = System.DateTime
type Duration = System.TimeSpan
#else
open NodaTime
open NodaTime.Text

type Instant = NodaTime.Instant
type Duration = NodaTime.Duration
#endif

#if FABLE_COMPILER
module TE = Thoth.Json.Encode
module TD = Thoth.Json.Decode
#else
module TE = Thoth.Json.Net.Encode
module TD = Thoth.Json.Net.Decode
#endif

open Dap.Prelude
open Dap.Context

type IClock =
    abstract Now : Instant with get
    abstract CalcDuration : Instant -> Instant * Duration
    abstract Now' : Instant with get
    abstract CalcDuration' : Instant -> Instant * Duration

let dateTimeUtcNow () = System.DateTime.UtcNow

#if FABLE_COMPILER
let inline getNow' () = System.DateTime.UtcNow
#else
let inline getNow' () = SystemClock.Instance.GetCurrentInstant ()

let toDateTimeUtc (time : Instant) =
    time.ToDateTimeUtc ()

let ofDateTimeUtc (dateTime : System.DateTime) =
    let utcDateTime, changed =
        if dateTime.Kind = DateTimeKind.Utc then
            dateTime, false
        elif dateTime.Kind = DateTimeKind.Unspecified then
            DateTime.SpecifyKind (dateTime, DateTimeKind.Utc), false
        else
            DateTime.SpecifyKind (dateTime, DateTimeKind.Utc), true
    if changed then
        logWarn (getLogging ()) "Clock.ofDateTimeUtc" "UTC_Conversion" (dateTime.Kind, dateTime, utcDateTime)
    Instant.FromDateTimeUtc utcDateTime
#endif

let private calcDuration (fromTime : Instant) =
    let now = getNow' ()
    (now, now - fromTime)

type RealClock () =
    interface IClock with
        member __.Now = getNow' ()
        member __.CalcDuration fromTime = calcDuration fromTime
        member __.Now' = getNow' ()
        member __.CalcDuration' fromTime = calcDuration fromTime

type FakeClock () =
    let mutable now = getNow' ()
    member this.Set (now' : Instant) = now <- now'
    member this.Add (duration : Duration) = now <- now + duration
    interface IClock with
        member this.Now = now
        member this.CalcDuration fromTime = (now, now - fromTime)
        member this.Now' = getNow' ()
        member __.CalcDuration' fromTime = calcDuration fromTime

let private TIMESTAMP_FORMAT = "yyyy-MM-ddTHH:mm:ss"

let dateTimeToText (time : DateTime) =
    time.ToString TIMESTAMP_FORMAT

#if FABLE_COMPILER
let dateTimeOfText (text : string) : Result<DateTime, exn> =
    try
        //Fable doesn't support ParseExact
        DateTime.Parse (text)
        |> Ok
    with e ->
        Error e

let noDuration = TimeSpan.Zero

#else
let dateTimeOfText (text : string) : Result<DateTime, exn> =
    try
        DateTime.ParseExact (text, TIMESTAMP_FORMAT, System.Globalization.CultureInfo.InvariantCulture)
        |> Ok
    with e ->
        Error e

// https://nodatime.org/2.4.x/api/NodaTime.Text.InstantPattern.html
// https://nodatime.org/2.4.x/userguide/instant-patterns
[<RequireQualifiedAccess>]
type InstantFormat =
    | General
    | Date
    | DateHour
    | DateHourMinute
    | DateHourMinuteSecond
    | DateHourMinuteSecond'
    | DateHourMinuteSecondSub
    | DateHourMinuteSecondSub'
    | Custom of string
with
    member this.Pattern =
        match this with
        | General -> InstantPattern.General
        | Date -> InstantPattern.CreateWithInvariantCulture "uuuu-MM-dd"
        | DateHour -> InstantPattern.CreateWithInvariantCulture "uuuu-MM-ddTHH"
        | DateHourMinute -> InstantPattern.CreateWithInvariantCulture "uuuu-MM-ddTHH:mm"
        | DateHourMinuteSecond -> InstantPattern.CreateWithInvariantCulture "uuuu-MM-ddTHH:mm:ss"
        | DateHourMinuteSecond' -> InstantPattern.CreateWithInvariantCulture "uuuu-MM-ddTHH_mm_ss"
        | DateHourMinuteSecondSub -> InstantPattern.CreateWithInvariantCulture "uuuu-MM-ddTHH:mm:ss;FFFFFFFFF"
        | DateHourMinuteSecondSub' -> InstantPattern.CreateWithInvariantCulture "uuuu-MM-ddTHH_mm_ss_FFFFFFFFF"
        | Custom format -> InstantPattern.CreateWithInvariantCulture format
    member this.Format instant = this.Pattern.Format instant
    member this.Parse text =
        let result = this.Pattern.Parse text
        if result.Success then
            Ok result.Value
        else
            Error result.Exception
    member this.JsonEncoder : JsonEncoder<Instant> =
        fun instant -> E.string <| this.Format instant
    member this.JsonDecoder : JsonDecoder<Instant> =
        fun path token ->
            if token.IsDate then
                TD.datetime path token
                |> Result.map ofDateTimeUtc
            elif token.IsString then
                this.Parse (token.ToStringValue ())
                |> Result.mapError (fun e ->
                    (path, JsonErrorReason.BadPrimitiveExtra ("a string of Instant", token, e.Message))
                )
            else
                Error (path, JsonErrorReason.BadPrimitive("a string of Instant", token))
    member this.JsonSpec =
        FieldSpec.Create<Instant> (this.JsonEncoder, this.JsonDecoder)

let instantToText (instant : Instant) =
    InstantFormat.General.Format instant

let instantOfText (text : string) : Result<Instant, exn> =
    InstantFormat.General.Parse text

let noDuration = Duration.Zero

// https://nodatime.org/2.4.x/api/NodaTime.Text.DurationPattern.html
// https://nodatime.org/2.4.x/userguide/duration-patterns
[<RequireQualifiedAccess>]
type DurationFormat =
    | RoundTrip
    | HourMinuteSecond
    | MinuteSecond
    | Second
    | Custom of string
with
    member this.Pattern =
        match this with
        | RoundTrip -> DurationPattern.Roundtrip
        | HourMinuteSecond -> DurationPattern.CreateWithInvariantCulture "-H:mm:ss.FFFFFFFFF"
        | MinuteSecond -> DurationPattern.CreateWithInvariantCulture "-M:ss.FFFFFFFFF"
        | Second -> DurationPattern.CreateWithInvariantCulture "-s.FFFFFFFFF"
        | Custom format -> DurationPattern.CreateWithInvariantCulture format
    member this.Format duration = this.Pattern.Format duration
    member this.Parse text =
        let result = this.Pattern.Parse text
        if result.Success then
            Ok result.Value
        else
            Error result.Exception
    member this.JsonEncoder : JsonEncoder<Duration> =
        fun duration -> E.string <| this.Format duration
    member this.JsonDecoder : JsonDecoder<Duration> =
        fun path token ->
            if token.IsTimeSpan then
                Ok <| Duration.FromTimeSpan (token.Value<TimeSpan> ())
            elif token.IsString then
                this.Parse (token.ToStringValue ())
                |> Result.mapError (fun e ->
                    (path, JsonErrorReason.BadPrimitiveExtra ("a string of Duration", token, e.Message))
                )
            else
                Error (path, JsonErrorReason.BadPrimitive("a string of Duration", token))
    member this.JsonSpec =
        FieldSpec.Create<Duration> (this.JsonEncoder, this.JsonDecoder)

let durationToText (duration : Duration) =
    DurationFormat.RoundTrip.Format duration

let durationOfText (text : string) : Result<Duration, exn> =
    DurationFormat.RoundTrip.Parse text
#endif

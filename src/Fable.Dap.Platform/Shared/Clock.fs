[<AutoOpen>]
module Dap.Platform.Clock

#if FABLE_COMPILER
open System

type Instant = System.DateTime
type Duration = System.TimeSpan
#else
open NodaTime
open NodaTime.Text

type Instant = NodaTime.Instant
type Duration = NodaTime.Duration
#endif

type IClock =
    abstract Now : Instant with get
    abstract CalcDuration : Instant -> Instant * Duration
    abstract Now' : Instant with get
    abstract CalcDuration' : Instant -> Instant * Duration

let dateTimeUtcNow = System.DateTime.UtcNow

#if FABLE_COMPILER
let inline getNow' () = dateTimeUtcNow
#else
let inline getNow' () = SystemClock.Instance.GetCurrentInstant ()
let toDateTimeUtc (time : Instant) =
    time.ToDateTimeUtc ()
#endif

let private calcDuration (fromTime : Instant) =
    let now = getNow' ()
    (now, now - fromTime)

type RealClock () =
    interface IClock with
        member _this.Now = getNow' ()
        member _this.CalcDuration fromTime = calcDuration fromTime
        member _this.Now' = getNow' ()
        member _this.CalcDuration' fromTime = calcDuration fromTime

type FakeClock () =
    let mutable now = getNow' ()
    member this.Set (now' : Instant) = now <- now'
    member this.Add (duration : Duration) = now <- now + duration
    interface IClock with
        member this.Now = now
        member this.CalcDuration fromTime = (now, now - fromTime)
        member this.Now' = getNow' ()
        member _this.CalcDuration' fromTime = calcDuration fromTime

#if FABLE_COMPILER
#else
let instantToText (instant : Instant) =
    InstantPattern.General.Format (instant)

let instantOfText (text : string) : Result<Instant, exn> =
    let result = InstantPattern.General.Parse text
    if result.Success then
        Ok result.Value
    else
        Error result.Exception

let instantToString (format : string) =
    let pattern = InstantPattern.CreateWithInvariantCulture (format)
    fun (time : Instant) -> pattern.Format (time)

type InstantFormat =
    | Date
    | DateHour
    | DateHourMinute
    | Custom of string
with
    member this.Format =
        match this with
        | Date -> instantToString "uuuu-MM-dd"
        | DateHour -> instantToString "uuuu-MM-ddTHH"
        | DateHourMinute -> instantToString "uuuu-MM-ddTHH:mm"
        | Custom format -> instantToString format

let noDuration = Duration.Zero
#endif
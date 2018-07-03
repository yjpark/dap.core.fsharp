[<AutoOpen>]
module Dap.Platform.Clock

// Follow the ideas from this blog
// https://hanson.io/taming-the-datetime-beast-with-noda-time/
//
// Use NodaTime to express time related data clearly
// Encapsulate now time into Env, so can support time travel kind
// operation, or provide time scale properly.

open NodaTime
open NodaTime.Text

type Instant = NodaTime.Instant
type Duration = NodaTime.Duration

type IClock =
    abstract Now : Instant with get
    abstract CalcDuration : Instant -> Instant * Duration
    abstract Now' : Instant with get
    abstract CalcDuration' : Instant -> Instant * Duration

let inline getNow' () =
    SystemClock.Instance.GetCurrentInstant ()

type RealClock () =
    interface IClock with
        member this.Now =
            getNow' ()
        member this.CalcDuration fromTime =
            let now = getNow' ()
            (now, now - fromTime)
        member this.Now' =
            getNow' ()
        member this.CalcDuration' fromTime =
            let now = getNow' ()
            (now, now - fromTime)

type FakeClock () =
    let mutable now = getNow' ()
    member this.Set (now' : Instant) =
        now <- now'
    member this.Add (duration : Duration) =
        now <- now + duration
    interface IClock with
        member this.Now = now
        member this.CalcDuration fromTime =
            (now, now - fromTime)
        member this.Now' =
            getNow' ()
        member this.CalcDuration' fromTime =
            let now = getNow' ()
            (now, now - fromTime)

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
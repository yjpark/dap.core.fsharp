[<AutoOpen>]
module Dap.Platform.Stats

type DurationStats<[<Measure>] 'u> = {
    SlowCap : float<'u>
    mutable SlowCount : int
    mutable TotalCount : int
    mutable TotalDuration : float<'u>
} with
    member this.AddDuration (duration : float<'u>) : bool =
        this.TotalCount <- this.TotalCount + 1
        this.TotalDuration <- this.TotalDuration + duration
        if duration > this.SlowCap then
            this.SlowCount <- this.SlowCount + 1
            true
        else
            false

type FuncStats<[<Measure>] 'u> = {
    mutable StartedCount : int
    mutable SucceedCount : int
    mutable FailedCount : int
    Duration : DurationStats<'u>
} with
    member this.IncStartedCount () : unit =
        this.StartedCount <- this.StartedCount + 1
    member this.IncSucceedCount () : unit =
        this.SucceedCount <- this.SucceedCount + 1
    member this.IncFailedCount () : unit =
        this.FailedCount <- this.FailedCount + 1

let secondOfDuration (duration : Duration) : float<second> =
    LanguagePrimitives.FloatWithMeasure <| duration.TotalSeconds

let msOfDuration (duration : Duration) : float<ms> =
    LanguagePrimitives.FloatWithMeasure <| duration.TotalMilliseconds

let durationStatsOfCap (cap : float<'u>) : DurationStats<'u> =
    {
        SlowCap = cap
        SlowCount = 0
        TotalCount = 0
        TotalDuration = LanguagePrimitives.FloatWithMeasure <| 0.0
    }

let funcStatsOfCap (cap : float<'u>) = {
    StartedCount = 0
    SucceedCount = 0
    FailedCount = 0
    Duration = durationStatsOfCap cap
}

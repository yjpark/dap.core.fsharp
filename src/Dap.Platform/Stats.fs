[<AutoOpen>]
module Dap.Platform.Stats

type DurationStats<[<Measure>] 'u> = {
    SlowCap : float<'u>
    mutable SlowCount : int
    mutable TotalCount : int
    mutable TotalDuration : float<'u>
}

type FuncStats<[<Measure>] 'u> = {
    mutable StartedCount : int
    mutable SucceedCount : int
    mutable FailedCount : int
    Duration : DurationStats<'u>
}
with 
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

let updateDurationStats (duration : float<'u>) (stats : DurationStats<'u>) : bool =
    stats.TotalCount <- stats.TotalCount + 1
    stats.TotalDuration <- stats.TotalDuration + duration
    if duration > stats.SlowCap then
        stats.SlowCount <- stats.SlowCount + 1
        true
    else
        false

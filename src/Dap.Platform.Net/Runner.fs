[<AutoOpen>]
module Dap.Platform.Runner

open System.Threading.Tasks
open Dap.Prelude

type StatsKind =
    | DeliverDuration
    | ProcessDuration
    | ReplyDuration
    | FuncDuration
    | TaskDuration

type GetSlowCap = StatsKind -> float<ms>

and Func<'res> = IRunner -> 'res
and OnFailed = IRunner -> exn -> unit
and GetTask<'res> = IRunner -> Task<'res>

and Func'<'runner, 'res> = IRunner<'runner> -> 'res
and OnFailed'<'runner> = IRunner<'runner> -> exn -> unit
and GetTask'<'runner, 'res> = IRunner<'runner> -> Task<'res>

and Stats = {
    Deliver : DurationStats<ms>
    Process : DurationStats<ms>
    Reply : FuncStats<ms>
    Func : FuncStats<ms>
    Task : FuncStats<ms>
}

and IRunner =
    inherit ILogger
    abstract Clock : IClock with get
    abstract Stats : Stats with get
    abstract RunFunc : Func<'res> -> Result<'res, exn>
    abstract RunTask : OnFailed -> GetTask<unit> -> unit
    //abstract AwaitTask : GetTask<'res> -> Result<'res, exn>

and IRunner<'runner> =
    inherit IRunner
    abstract Self : 'runner with get
    abstract RunFunc' : Func'<'runner, 'res> -> Result<'res, exn>
    abstract RunTask' : OnFailed'<'runner> -> GetTask'<'runner, unit> -> unit
    //abstract AwaitTask' : GetTask'<'runner, 'res> -> Result<'res, exn>

let statsOfCap (getSlowCap : GetSlowCap) : Stats = {
    Deliver = durationStatsOfCap <| getSlowCap DeliverDuration
    Process = durationStatsOfCap <| getSlowCap ProcessDuration
    Reply = funcStatsOfCap <| getSlowCap ReplyDuration
    Func = funcStatsOfCap <| getSlowCap FuncDuration
    Task = funcStatsOfCap <| getSlowCap TaskDuration
}

let getRemoteSlowCap replySlowCap = 
    function
    | DeliverDuration -> DefaultDeliverSlowCap
    | ProcessDuration -> DefaultProcessSlowCap
    | ReplyDuration -> replySlowCap
    | FuncDuration -> DefaultFuncSlowCap
    | TaskDuration -> DefaultTaskSlowCap

let getDefaultSlowCap = 
    getRemoteSlowCap DefaultReplySlowCap

let private tplRunFuncSucceed = LogEvent.Template4<string, string, Duration, obj>(AckLogLevel, "[{Section}] {Func} {Duration} ~> Succeed: {Res}")
let private tplSlowRunFuncSucceed = LogEvent.Template4<string, string, Duration, obj>(LogLevelWarning, "[{Section}] {Func} {Duration} ~> Succeed: {Res}")
let private tplRunFuncFailed = LogEvent.Template3WithException<string, string, Duration>(LogLevelError, "[{Section}] {Func} {Duration} ~> Failed")
let private tplSlowFuncStats = LogEvent.Template4<string, float<ms>, string, DurationStats<ms>>(LogLevelWarning, "[{Section}] {Duration}<ms> {Func} ~> {Detail}")

let trackDurationStatsInMs (runner : IRunner) (fromTime : Instant)
                            (stats : DurationStats<ms>)
                            (getSlowMessage : float<ms> * DurationStats<ms> -> LogEvent)
                            : Instant * Duration * float<ms> * bool =
    let (time, duration) = runner.Clock.CalcDuration' fromTime
    let durationInMs = msOfDuration duration
    let isSlow = updateDurationStats durationInMs stats
    if isSlow then
        runner.Log <| getSlowMessage (durationInMs, stats)
    (time, duration, durationInMs, isSlow)

let private getSlowTaskMessage (section : string)
                                (getTask : string) (duration, stats) =
    tplSlowFuncStats ("Slow_" + section) duration getTask stats

let private logRunResult (runner : IRunner) (section : string)
                            (stats : FuncStats<ms>)
                            (func : string) (startTime : Instant) (result : Result<'res, exn>) =
    let (_, duration, _, isSlow) = trackDurationStatsInMs runner startTime stats.Duration (getSlowTaskMessage section func)
    match result with
    | Ok res ->
        stats.IncSucceedCount ()
        let tpl = if isSlow then tplSlowRunFuncSucceed else tplRunFuncSucceed
        runner.Log <| tpl section func duration res
    | Result.Error e ->
        stats.IncFailedCount ()
        runner.Log <| tplRunFuncFailed section func duration e
    result

let internal runFunc' (runner : IRunner) (func : Func<'res>) : Result<'res, exn> =
    let time = runner.Clock.Now'
    runner.Stats.Func.IncStartedCount ()
    try
        let res = func runner
        Ok res
    with
    | e ->
        Result.Error e
    |> logRunResult runner "RunFunc" runner.Stats.Func (func.ToString()) time

let internal runFunc'' (runner : IRunner<'runner>) (func : Func'<'runner, 'res>) : Result<'res, exn> =
    let time = runner.Clock.Now'
    runner.Stats.Func.IncStartedCount ()
    try
        let res = func runner
        Ok res
    with
    | e ->
        Result.Error e
    |> logRunResult runner "RunFunc'" runner.Stats.Func (func.ToString()) time

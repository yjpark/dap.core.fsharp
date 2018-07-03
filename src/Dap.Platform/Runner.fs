[<AutoOpen>]
module Dap.Platform.Runner

open System.Threading
open System.Threading.Tasks
open Dap.Prelude

type StatsKind =
    | DeliverDuration
    | ProcessDuration
    | ReplyDuration
    | FuncDuration
    | TaskDuration

type GetSlowCap = StatsKind -> float<ms>

and Func<'runner, 'res> = 'runner -> 'res
and OnFailed<'runner> = 'runner -> exn -> unit
and GetTask<'runner, 'res> = 'runner -> Task<'res>

and Stats = {
    Deliver : DurationStats<ms>
    Process : DurationStats<ms>
    Reply : FuncStats<ms>
    Func : FuncStats<ms>
    Task : FuncStats<ms>
}

type IPendingTask =
    abstract Run : CancellationToken -> Task option

and IRunner =
    inherit ILogger
    abstract Clock : IClock with get
    abstract Stats : Stats with get
    abstract RunFunc<'res> : Func<IRunner, 'res> -> Result<'res, exn>
    abstract AddTask : OnFailed<IRunner> -> GetTask<IRunner, unit> -> unit
    abstract ScheduleTask : IPendingTask -> unit
    abstract RunTasks : unit -> int
    abstract ClearPendingTasks : unit -> int
    abstract CancelRunningTasks : unit -> int
    abstract PendingTasksCount : int with get
    abstract RunningTasksCount : int with get

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

let trackDurationStatsInMs (runner : 'runner when 'runner :> IRunner ) (fromTime : Instant)
                            (stats : DurationStats<ms>)
                            (getSlowMessage : float<ms> * DurationStats<ms> -> LogEvent)
                            : Instant * Duration * float<ms> * bool =
    let (time, duration) = runner.Clock.CalcDuration' fromTime
    let durationInMs = msOfDuration duration
    let isSlow = stats.AddDuration durationInMs
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

let ignoreOnFailed : OnFailed<'runner> =
    fun _runner _e -> ()

let internal runFunc' (runner : 'runner when 'runner :> IRunner) (func : Func<'runner, 'res>) : Result<'res, exn> =
    let time = runner.Clock.Now'
    runner.Stats.Func.IncStartedCount ()
    try
        let res = func runner
        Ok res
    with
    | e ->
        Error e
    |> logRunResult runner "RunFunc'" runner.Stats.Func (func.ToString()) time
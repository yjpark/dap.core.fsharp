[<AutoOpen>]
module Dap.Platform.Tasks

open System.Threading.Tasks
open Dap.Prelude

let private tplRunTaskSucceed = LogEvent.Template3<string, string, Duration>(AckLogLevel, "[{Section}] {Task} {Duration} ~> Succeed")
let private tplRunTaskSucceed' = LogEvent.Template4<string, string, Duration, obj>(AckLogLevel, "[{Section}] {Task} {Duration} ~> Succeed: {Res}")
let private tplRunTaskFailed = LogEvent.Template3WithException<string, string, Duration>(LogLevelError, "[{Section}] {Task} {Duration} ~> Failed")
let private tplSlowTaskStats = LogEvent.Template4<string, float<ms>, string, DurationStats<ms>>(LogLevelWarning, "[{Section}] {Duration}<ms> {Task} ~> {Detail}")

let internal dispatchAsync' (dispatcher : IDispatcher<'msg>) (getMsg : Callback<'res> -> 'msg) : Task<'res> =
    let onDone = new TaskCompletionSource<'res>();
    let callback = callbackAsync dispatcher onDone
    let msg = getMsg callback
    dispatch' dispatcher msg
    onDone.Task

let private getSlowTaskMessage (section : string)
                                (getTask : string) (duration, stats) =
    tplSlowTaskStats ("Slow_" + section) duration getTask stats

let private logRunResult (runner : IRunner) (section : string)
                            (stats : FuncStats<ms>)
                            (getTask : string) (startTime : Instant) (result : exn option) =
    let (_, duration, _, _) = trackDurationStatsInMs runner startTime stats.Duration (getSlowTaskMessage section getTask)
    match result with
    | None ->
        stats.SucceedCount <- stats.SucceedCount + 1
        runner.Log <| tplRunTaskSucceed section getTask duration
    | Some e ->
        stats.FailedCount <- stats.FailedCount + 1
        runner.Log <| tplRunTaskFailed section getTask duration e
    result

let private logRunResult' runner section states getTask startTime (onFailed : exn -> unit) result =
    match logRunResult runner section states getTask startTime result with
    | None -> ()
    | Some e -> onFailed e

let private tryWaitTask (runner : 'runner) (getTask : GetTask<'runner, unit>) : exn option = 
    try
        let task = getTask runner
        task.Wait()
        None
    with
    | e ->
        Some e

let private tryRunTask (runner : 'runner) (getTask : GetTask<'runner, unit>) (onDone : exn option -> unit) : unit =
    Task.Run(fun () -> tryWaitTask runner getTask |> onDone)
    |> ignore

let internal runTask' (runner : 'runner when 'runner :> IRunner) (onFailed : OnFailed<'runner>) (getTask : GetTask<'runner, unit>) : unit =
    let time = runner.Clock.Now'
    runner.Stats.Task.StartedCount <- runner.Stats.Task.StartedCount + 1
    (logRunResult' runner "RunTask'" runner.Stats.Task (getTask.ToString()) time (onFailed runner))
    |> tryRunTask runner getTask

let ignoreOnFailed : OnFailed<'runner> =
    fun _runner _e -> ()
[<AutoOpen>]
module Dap.Platform.Tasks

open System.Threading.Tasks
open Dap.Prelude

let private tplRunTaskSucceed = LogEvent.Template3<string, string, Duration>(AckLogLevel, "[{Section}] {Task} {Duration} ~> Succeed")
let private tplRunTaskSucceed' = LogEvent.Template4<string, string, Duration, obj>(AckLogLevel, "[{Section}] {Task} {Duration} ~> Succeed: {Res}")
let private tplRunTaskFailed = LogEvent.Template3WithException<string, string, Duration>(LogLevelError, "[{Section}] {Task} {Duration} ~> Failed")
let private tplSlowTaskStats = LogEvent.Template4<string, float<ms>, string, DurationStats<ms>>(LogLevelWarning, "[{Section}] {Duration}<ms> {Task} ~> {Detail}")

let private waitTask (task : Task) : unit =
    task.Wait()

let private tryWaitTask (task : Task) : exn option = 
    try
        task.Wait()
        None
    with
    | e ->
        Some e

let private runTask (task : Task) : unit =
    Task.Run(fun () -> waitTask task)
    |> ignore

let private tryRunTask (onDone : exn option -> unit) (task : Task) : unit =
    Task.Run(fun () -> tryWaitTask task |> onDone)
    |> ignore

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

let internal runTask' (runner : IRunner) (onFailed : OnFailed) (getTask : GetTask<unit>) : unit =
    let time = runner.Clock.Now'
    runner.Stats.Task.StartedCount <- runner.Stats.Task.StartedCount + 1
    getTask runner
    |> tryRunTask (logRunResult' runner "RunTask" runner.Stats.Task (getTask.ToString()) time (onFailed runner))

let internal runTask'' (runner : IRunner<'runner>) (onFailed : OnFailed'<'runner>) (getTask : GetTask'<'runner, unit>) : unit =
    let time = runner.Clock.Now'
    runner.Stats.Task.StartedCount <- runner.Stats.Task.StartedCount + 1
    getTask runner
    |> tryRunTask (logRunResult' runner "RunTask'" runner.Stats.Task (getTask.ToString()) time (onFailed runner))

(* Note: These AwaitTask way is not working, since dotnet is not happy with mixing 
 * async codes and sync codes, did try with some async tricks here, still no luck
 * the main reason for this is to initialize agent reference in Logic.Init, can eliminate
 * some mutable state usage, leave the codes here in case want to give further tries in
 * the future

let private logRunResult'' (runner : IRunner) (section : string)
                            (stats : FuncStats<ms>)
                            (getTask : string) (startTime : Instant) 
                            (result : Result<'res, exn>) =
    let (_, duration, _, _) = trackDurationStatsInMs runner startTime stats.Duration (getSlowTaskMessage section getTask)
    match result with
    | Ok res ->
        stats.SucceedCount <- stats.SucceedCount + 1
        runner.Log <| tplRunTaskSucceed' section getTask duration res
    | Error e ->
        stats.FailedCount <- stats.FailedCount + 1
        runner.Log <| tplRunTaskFailed section getTask duration e
    result
    
let private tryRunTask' (runner : IRunner) (getTask : GetTask<'res>) : Async<Result<'res, exn>> = async {
    try
        let original = System.Threading.SynchronizationContext.Current
        do! Async.SwitchToNewThread()
        let task = getTask runner
        let! res = Async.AwaitTask task
        do! Async.SwitchToContext(original) 
        return (Ok res)
    with
    | e ->
        return (Error e)
}

let private tryRunTask'' (runner : IRunner<'runner>) (getTask : GetTask'<'runner, 'res>) : Async<Result<'res, exn>> = async {
    try
        let original = System.Threading.SynchronizationContext.Current
        do! Async.SwitchToNewThread()
        let task = getTask runner
        let! res = Async.AwaitTask task
        do! Async.SwitchToContext(original) 
        return (Ok res)
    with
    | e ->
        return (Error e)
}

let internal awaitTask' (runner : IRunner) (getTask : GetTask<'res>) : Result<'res, exn> =
    let time = runner.Clock.Now'
    runner.Stats.Task.StartedCount <- runner.Stats.Task.StartedCount + 1
    tryRunTask' runner getTask
    |> Async.RunSynchronously
    |> logRunResult'' runner "AwaitTask" runner.Stats.Task (getTask.ToString()) time

let internal awaitTask'' (runner : IRunner<'runner>) (getTask : GetTask'<'runner, 'res>) : Result<'res, exn> =
    let time = runner.Clock.Now'
    runner.Stats.Task.StartedCount <- runner.Stats.Task.StartedCount + 1
    tryRunTask'' runner getTask
    |> Async.RunSynchronously
    |> logRunResult'' runner "AwaitTask'" runner.Stats.Task (getTask.ToString()) time
*)

let ignoreOnFailed : OnFailed =
    fun _runner _e -> ()

let ignoreOnFailed' : OnFailed'<'runner> =
    fun _runner _e -> ()

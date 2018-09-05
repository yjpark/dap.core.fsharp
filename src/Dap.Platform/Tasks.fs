[<AutoOpen>]
module Dap.Platform.Tasks

open System.Threading
open System.Threading.Tasks
open FSharp.Control.Tasks.V2
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

(*
let private tryRunTask (runner : 'runner) (getTask : GetTask<'runner, unit>) (onDone : exn option -> unit) : unit =
    let getTask' : unit -> Task<unit> = fun () -> task {
        do! Task.Yield()
        try
            do! (getTask runner)
            onDone None
        with
        | e ->
            onDone <| Some e
    }
    Task.Run (fun () -> (getTask' ()) :> Task)
    |> ignore
*)

type internal PendingTask<'runner> when 'runner :> IRunner = {
    Runner : 'runner
    OnFailed : OnFailed<'runner>
    GetTask : GetTask<'runner, unit>
} with
    static member Create runner onFailed getTask =
        {
            Runner = runner
            OnFailed = onFailed
            GetTask = getTask
        }
    interface IPendingTask with
        member this.Run (cancellationToken : CancellationToken) =
            let runner = this.Runner
            let time = runner.Clock.Now'
            runner.Stats.Task.StartedCount <- this.Runner.Stats.Task.StartedCount + 1
            let onDone = logRunResult' runner "RunTask" runner.Stats.Task (this.GetTask.ToString()) time (this.OnFailed runner)
            let runTask = fun () ->
                try
                    let task = this.GetTask runner
                    task.Wait()
                    onDone None
                with
                | e ->
                    onDone <| Some e
            try
                Task.Run (runTask, cancellationToken)
                |> Some
            with e ->
                onDone <| Some e
                None

let addTask' (runner : 'runner when 'runner :> IRunner) (onFailed : OnFailed<'runner>) (getTask : GetTask<'runner, unit>) : unit =
    PendingTask<'runner>.Create runner onFailed getTask
    |> (runner :> ITaskManager).ScheduleTask

let runTask' (runner : 'runner when 'runner :> IRunner) (onFailed : OnFailed<'runner>) (getTask : GetTask<'runner, unit>) : unit =
    PendingTask<'runner>.Create runner onFailed getTask
    |> (runner :> ITaskManager).StartTask

type internal TaskManager () =
    let mutable runningTasks : (CancellationTokenSource * Task) list = []
    let mutable pendingTasks : IPendingTask list = []

    let startTask (pendingTask : IPendingTask) (count : int) : int =
        let cts = new CancellationTokenSource()
        match pendingTask.Run cts.Token with
        | None ->
            count
        | Some task ->
            runningTasks <- (cts, task) :: runningTasks
            count + 1

    let cancelTask ((cts, task) : CancellationTokenSource * Task) (count : int) : int =
        cts.Cancel ()
        count + 1

    let removeCompletedTasks () =
        runningTasks <- runningTasks |> List.filter (fun (cts, task) -> not task.IsCompleted)

    interface ITaskManager with
        member __.StartTask (task : IPendingTask) =
            startTask task 0 |> ignore
        member __.ScheduleTask (task : IPendingTask) =
            pendingTasks <- task :: pendingTasks
        member this.PendingTasksCount =
            pendingTasks.Length
        member __.StartPendingTasks () =
            let tasks = pendingTasks
            pendingTasks <- []
            removeCompletedTasks ()
            List.foldBack startTask tasks 0
        member __.ClearPendingTasks () =
            let tasks = pendingTasks
            pendingTasks <- []
            tasks.Length
        member this.RunningTasksCount =
            removeCompletedTasks ()
            runningTasks.Length
        member __.CancelRunningTasks () =
            removeCompletedTasks ()
            let tasks = runningTasks
            runningTasks <- []
            List.foldBack cancelTask tasks 0

type System.Threading.Tasks.Task with
    static member Delay (delay : Duration) =
        if delay.TotalMilliseconds > 0.0 then
            Task.Delay (int delay.TotalMilliseconds)
        else
            Task.CompletedTask
    static member Delay (delay : float<second>) =
        if delay > 0.0<second> then
            Task.Delay (int (System.Math.Round (1000.0 * float delay)))
        else
            Task.CompletedTask
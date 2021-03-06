[<AutoOpen>]
module Dap.Platform.Runner

open System.Threading
open System.Threading.Tasks

open Dap.Prelude
open Dap.Context

type Func<'runner, 'res> = 'runner -> 'res
type OnFailed<'runner> = 'runner -> exn -> unit
type GetTask<'runner, 'res> = 'runner -> Task<'res>

type IPendingTask =
    abstract Run : CancellationToken -> Task option

and ITaskManager =
    abstract StartTask : IPendingTask -> unit
    abstract ScheduleTask : IPendingTask -> unit
    abstract PendingTasksCount : int with get
    abstract StartPendingTasks : unit -> int
    abstract ClearPendingTasks : unit -> int
    abstract RunningTasksCount : int with get
    abstract CancelRunningTasks : unit -> int

and IRunner =
    inherit ILogger
    inherit ITaskManager
    abstract Clock : IClock with get
    abstract Dash0 : IDash with get
    abstract RunFunc0<'res> : Func<IRunner, 'res> -> Result<'res, exn>
    abstract AddTask0 : OnFailed<IRunner> -> GetTask<IRunner, unit> -> unit
    abstract RunTask0 : OnFailed<IRunner> -> GetTask<IRunner, unit> -> unit

and IRunner<'runner when 'runner :> IRunner> =
    abstract Runner : 'runner with get
    abstract RunFunc<'res> : Func<'runner, 'res> -> Result<'res, exn>
    abstract AddTask : OnFailed<'runner> -> GetTask<'runner, unit> -> unit
    abstract RunTask : OnFailed<'runner> -> GetTask<'runner, unit> -> unit

let private tplRunFuncSucceed = LogEvent.Template4<string, string, Duration, obj>(AckLogLevel, "[{Section}] {Func} {Duration} ~> Succeed: {Res}")
let private tplSlowRunFuncSucceed = LogEvent.Template4<string, string, Duration, obj>(LogLevelInformation, "[{Section}] {Func} {Duration} ~> Succeed: {Res}")
let private tplRunFuncFailed = LogEvent.Template3WithException<string, string, Duration>(LogLevelWarning, "[{Section}] {Func} {Duration} ~> Failed")
let private tplSlowFuncStats = LogEvent.Template5<string, Duration, string, string, string>(LogLevelInformation, "[{Section}] {Duration} {Func} ~> {Detail}\n{StackTrace}")

type DurationStats with
    static member AddOp' (op : string)
            (slowCap : IVarProperty<Duration>)
            (totalCount : IVarProperty<int>)
            (slowCount : IVarProperty<int>)
            (getStackTrace : unit -> string)
            (runner : 'runner when 'runner :> IRunner) (msg : string) (startTime : Instant) =
        let (time, duration) = runner.Clock.CalcDuration' startTime
        let isSlow = duration > slowCap.Value
        let opLog =
            if isSlow then
                let stackTrace = getStackTrace ()
                let opLog = OpLog.Create (op, msg, startTime, duration, stackTrace)
                slowCount.SetValue (slowCount.Value + 1)
                Some opLog
            else
                None
        totalCount.SetValue (totalCount.Value + 1)
        (duration, opLog)
    member this.AddOp (runner : 'runner when 'runner :> IRunner) (msg : string) (startTime : Instant) =
        DurationStats.AddOp' this.Spec.Key this.SlowCap this.TotalCount this.SlowCount (fun () ->
            (System.Diagnostics.StackTrace(2)) .ToString()
        ) runner msg startTime

type FuncStats with
    member this.AddResult (runner : 'runner when 'runner :> IRunner) (msg : string) (startTime : Instant) (result : Result<'res, exn>) =
        let (duration, slowOp) =
            DurationStats.AddOp' this.Spec.Key this.SlowCap this.TotalCount this.SlowCount (fun () ->
                match result with
                | Ok _res ->
                    (System.Diagnostics.StackTrace(2)) .ToString()
                | Result.Error e ->
                    sprintf "%s\n%s" e.Message e.StackTrace
            ) runner msg startTime
        let failedOp =
            match result with
            | Ok _res ->
                this.AddSucceedOp ()
            | Result.Error e ->
                let stackTrace = sprintf "%s\n%s" e.Message e.StackTrace
                this.AddFailedOp msg startTime duration stackTrace
        (duration, slowOp, failedOp)

let private logRunResult (runner : 'runner when 'runner :> IRunner)
            (msg : string) (startTime : Instant) (result : Result<'res, exn>) =
    let stats = runner.Dash0.Stats.Func
    let op = stats.Spec.Key
    let (duration, slowOp, _failedOp) = stats.AddResult runner msg startTime result
    slowOp
    |> Option.iter (fun opLog ->
        runner.Log <| tplSlowFuncStats ("Slow_" + op) duration msg (stats.ToLogDetail ()) opLog.StackTrace
    )
    match result with
    | Ok res ->
        if slowOp.IsSome then
            runner.Log <| tplSlowRunFuncSucceed op msg duration res
        else
            runner.Log <| tplRunFuncSucceed op msg duration res
    | Result.Error e ->
        runner.Log <| tplRunFuncFailed op msg duration e
    result

let ignoreOnFailed : OnFailed<'runner> =
    fun _runner _e -> ()

let raiseOnFailed : OnFailed<'runner> =
    fun _runner e -> raise e

let runFunc' (runner : 'runner when 'runner :> IRunner) (func : Func<'runner, 'res>) : Result<'res, exn> =
    let time = runner.Clock.Now'
    (runner :> IRunner).Dash0.Stats.Func.IncPendingCount ()
    try
        let res = func runner
        Ok res
    with
    | e ->
        Error e
    |> logRunResult runner (func.ToString()) time

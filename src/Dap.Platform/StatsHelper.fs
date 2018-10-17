[<AutoOpen>]
module Dap.Platform.StatsHelper

open Dap.Prelude
open Dap.Context

type DurationStats with
    static member AddOp' (op : string)
            (slowCap : IVarProperty<Duration>)
            (totalCount : IVarProperty<int>)
            (slowCount : IVarProperty<int>)
            (slowOps : IListProperty<IVarProperty<OpLog>>)
            (getStackTrace : unit -> string)
            (clock : IClock) (msg : string) (startTime : Instant) =
        let (time, duration) = clock.CalcDuration' startTime
        let isSlow = duration > slowCap.Value
        let opLog =
            if isSlow then
                let stackTrace = getStackTrace ()
                let opLog = OpLog.Create (op, msg, startTime, duration, stackTrace)
                (slowOps.Add ()) .SetValue opLog
                slowCount.SetValue (slowCount.Value + 1)
                Some opLog
            else
                None
        totalCount.SetValue (totalCount.Value + 1)
        (duration, opLog)
    member this.AddOp (clock : IClock) (msg : string) (startTime : Instant) =
        DurationStats.AddOp' this.Spec.Key this.SlowCap this.TotalCount this.SlowCount this.SlowOps (fun () ->
            (System.Diagnostics.StackTrace(2)) .ToString()
        ) clock msg startTime
    member this.ToLogDetail () =
        sprintf "[Total: %d, Slow: %d, SlowCap: %s]"
            this.TotalCount.Value this.SlowCount.Value (DurationFormat.Second.Format this.SlowCap.Value)
    member this.ClearLogs () =
        this.SlowOps.Clear () |> ignore

type FuncStats with
    member this.IncPendingCount () =
        this.PendingCount.SetValue (this.PendingCount.Value + 1)
    member this.AddSucceedOp () =
        this.SucceedCount.SetValue (this.SucceedCount.Value + 1)
        this.PendingCount.SetValue (this.PendingCount.Value - 1)
        None
    member this.AddFailedOp
            (msg : string) (startTime : Instant) (duration : Duration) (stackTrace) =
        let opLog = OpLog.Create (this.Spec.Key, msg, startTime, duration, stackTrace)
        (this.FailedOps.Add ()) .SetValue opLog
        this.FailedCount.SetValue (this.FailedCount.Value + 1)
        this.PendingCount.SetValue (this.PendingCount.Value - 1)
        Some opLog
    member this.AddResult (clock : IClock) (msg : string) (startTime : Instant) (result : Result<'res, exn>) =
        let (duration, slowOp) =
            DurationStats.AddOp' this.Spec.Key this.SlowCap this.TotalCount this.SlowCount this.SlowOps (fun () ->
                match result with
                | Ok _res ->
                    (System.Diagnostics.StackTrace(2)) .ToString()
                | Result.Error e ->
                    sprintf "%s\n%s" e.Message e.StackTrace
            ) clock msg startTime
        let failedOp =
            match result with
            | Ok _res ->
                this.AddSucceedOp ()
            | Result.Error e ->
                let stackTrace = sprintf "%s\n%s" e.Message e.StackTrace
                this.AddFailedOp msg startTime duration stackTrace
        (duration, slowOp, failedOp)
    member this.ToLogDetail () =
        sprintf "[Pending:%d, Succeed: %d, Failed: %d, Total: %d, Slow: %d, SlowCap: %s]"
            this.PendingCount.Value this.SucceedCount.Value this.FailedCount.Value
            this.TotalCount.Value this.SlowCount.Value (DurationFormat.Second.Format this.SlowCap.Value)
    member this.ClearLogs () =
        this.SlowOps.Clear () |> ignore
        this.FailedOps.Clear () |> ignore


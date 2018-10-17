[<AutoOpen>]
module Dap.Platform.StatsHelper

open Dap.Prelude
open Dap.Context

type DurationStats with
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
    member this.ToLogDetail () =
        sprintf "[Pending:%d, Succeed: %d, Failed: %d, Total: %d, Slow: %d, SlowCap: %s]"
            this.PendingCount.Value this.SucceedCount.Value this.FailedCount.Value
            this.TotalCount.Value this.SlowCount.Value (DurationFormat.Second.Format this.SlowCap.Value)
    member this.ClearLogs () =
        this.SlowOps.Clear () |> ignore
        this.FailedOps.Clear () |> ignore


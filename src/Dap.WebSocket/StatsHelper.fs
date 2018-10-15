[<AutoOpen>]
module Dap.WebSocket.StatsHelper

open Dap.Prelude
open Dap.Context
open Dap.Platform

let private tplSlowPktStats = LogEvent.Template5<string, Duration, int, string, string>(LogLevelWarning, "[{Section}] {Duration} {Bytes} ~> {Detail}\n{StackTrace}")

type TrafficStats with
    member this.IncPendingCount () =
        this.PendingCount.SetValue (this.PendingCount.Value + 1)
    member this.AddSucceedOp () =
        this.SucceedCount.SetValue (this.SucceedCount.Value + 1)
        this.PendingCount.SetValue (this.PendingCount.Value - 1)
        None
    member this.AddFailedOp (bytes : int) (startTime : Instant) (duration : Duration) (stackTrace) =
        let pktLog = PktLog.Create bytes startTime duration stackTrace
        (this.FailedPkts.Add ()) .SetValue pktLog
        this.FailedCount.SetValue (this.FailedCount.Value + 1)
        this.PendingCount.SetValue (this.PendingCount.Value - 1)
        Some pktLog
    member this.ToLogDetail () =
        sprintf "[Pending:%d, Succeed: %d, Failed: %d, Total: %d, Slow: %d, SlowCap: %s]"
            this.PendingCount.Value this.SucceedCount.Value this.FailedCount.Value
            this.TotalCount.Value this.SlowCount.Value (DurationFormat.Second.Format this.SlowCap.Value)
    member this.AddPkt (runner : IRunner) (bytes : int) (startTime : Instant) (err : exn option) =
        let getStackTrace = fun () ->
            match err with
            | Some e ->
                sprintf "%s\n%s" e.Message e.StackTrace
            | None ->
                (System.Diagnostics.StackTrace(2)) .ToString()
        let (_time, duration) = runner.Clock.CalcDuration' startTime
        let isSlow = duration > this.SlowCap.Value
        let slowPkt =
            if isSlow then
                let stackTrace = getStackTrace ()
                let pktLog = PktLog.Create bytes startTime duration stackTrace
                (this.SlowPkts.Add ()) .SetValue pktLog
                this.SlowCount.SetValue (this.SlowCount.Value + 1)
                Some pktLog
            else
                None
        this.TotalCount.SetValue (this.TotalCount.Value + 1)
        let failedPkt =
            match err with
            | None ->
                this.AddSucceedOp ()
            | Some _err ->
                this.AddFailedOp bytes startTime duration <| getStackTrace ()
        slowPkt
        |> Option.iter (fun pktLog ->
            runner.Log <| tplSlowPktStats this.Spec.Key duration bytes (this.ToLogDetail ()) pktLog.StackTrace
        )
        (duration, slowPkt, failedPkt)

type LinkStats with
    member this.AddStatus (runner : IRunner) (status : LinkStatus) =
        this.Status.SyncTo (this.StatusHistory.Add ())
        this.Status.SetValue <| StatusLog.Create runner.Clock.Now' status
        |> ignore

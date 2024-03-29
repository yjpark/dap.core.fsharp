[<AutoOpen>]
module Dap.WebSocket.Stats

open Dap.Prelude
open Dap.Context
open Dap.Platform

(*
 * Generated: <Record>
 *     IsJson, IsLoose
 *)
type PktLog = {
    Bytes : (* PktLog *) int
    Time : (* PktLog *) Instant
    Duration : (* PktLog *) Duration
    StackTrace : (* PktLog *) string
} with
    static member Create
        (
            ?bytes : (* PktLog *) int,
            ?time : (* PktLog *) Instant,
            ?duration : (* PktLog *) Duration,
            ?stackTrace : (* PktLog *) string
        ) : PktLog =
        {
            Bytes = (* PktLog *) bytes
                |> Option.defaultWith (fun () -> 0)
            Time = (* PktLog *) time
                |> Option.defaultWith (fun () -> (getNow' ()))
            Duration = (* PktLog *) duration
                |> Option.defaultWith (fun () -> noDuration)
            StackTrace = (* PktLog *) stackTrace
                |> Option.defaultWith (fun () -> "")
        }
    static member SetBytes ((* PktLog *) bytes : int) (this : PktLog) =
        {this with Bytes = bytes}
    static member SetTime ((* PktLog *) time : Instant) (this : PktLog) =
        {this with Time = time}
    static member SetDuration ((* PktLog *) duration : Duration) (this : PktLog) =
        {this with Duration = duration}
    static member SetStackTrace ((* PktLog *) stackTrace : string) (this : PktLog) =
        {this with StackTrace = stackTrace}
    static member JsonEncoder : JsonEncoder<PktLog> =
        fun (this : PktLog) ->
            E.object [
                yield "bytes", E.int (* PktLog *) this.Bytes
                yield "time", E.instant (* PktLog *) this.Time
                yield "duration", DurationFormat.Second.JsonEncoder (* PktLog *) this.Duration
                yield "stack_trace", E.string (* PktLog *) this.StackTrace
            ]
    static member JsonDecoder : JsonDecoder<PktLog> =
        D.object (fun get ->
            {
                Bytes = get.Optional.Field (* PktLog *) "bytes" D.int
                    |> Option.defaultValue 0
                Time = get.Optional.Field (* PktLog *) "time" D.instant
                    |> Option.defaultValue (getNow' ())
                Duration = get.Optional.Field (* PktLog *) "duration" DurationFormat.Second.JsonDecoder
                    |> Option.defaultValue noDuration
                StackTrace = get.Optional.Field (* PktLog *) "stack_trace" D.string
                    |> Option.defaultValue ""
            }
        )
    static member JsonSpec =
        FieldSpec.Create<PktLog> (PktLog.JsonEncoder, PktLog.JsonDecoder)
    interface IJson with
        member this.ToJson () = PktLog.JsonEncoder this
    interface IObj
    member this.WithBytes ((* PktLog *) bytes : int) =
        this |> PktLog.SetBytes bytes
    member this.WithTime ((* PktLog *) time : Instant) =
        this |> PktLog.SetTime time
    member this.WithDuration ((* PktLog *) duration : Duration) =
        this |> PktLog.SetDuration duration
    member this.WithStackTrace ((* PktLog *) stackTrace : string) =
        this |> PktLog.SetStackTrace stackTrace

(*
 * Generated: <Combo>
 *     IsFinal
 *)
type TrafficStats (owner : IOwner, key : Key) =
    inherit WrapProperties<TrafficStats, IComboProperty> ()
    let target' = Properties.combo (owner, key)
    let slowCap = target'.AddVar<(* TrafficStats *) Duration> (DurationFormat.Second.JsonEncoder, DurationFormat.Second.JsonDecoder, "slow_cap", DefaultPktSlowCap, None)
    let totalCount = target'.AddVar<(* TrafficStats *) int> (E.int, D.int, "total_count", 0, None)
    let slowCount = target'.AddVar<(* TrafficStats *) int> (E.int, D.int, "slow_count", 0, None)
    let pendingCount = target'.AddVar<(* TrafficStats *) int> (E.int, D.int, "pending_count", 0, None)
    let succeedCount = target'.AddVar<(* TrafficStats *) int> (E.int, D.int, "succeed_count", 0, None)
    let failedCount = target'.AddVar<(* TrafficStats *) int> (E.int, D.int, "failed_count", 0, None)
    let failedPkts = target'.AddList<(* TrafficStats *) PktLog> (PktLog.JsonEncoder, PktLog.JsonDecoder, "failed_pkts", (PktLog.Create ()), None)
    do (
        target'.SealCombo ()
        base.Setup (target')
    )
    static member Create (o, k) = new TrafficStats (o, k)
    static member Create () = TrafficStats.Create (noOwner, NoKey)
    static member AddToCombo key (combo : IComboProperty) =
        combo.AddCustom<TrafficStats> (TrafficStats.Create, key)
    override this.Self = this
    override __.Spawn (o, k) = TrafficStats.Create (o, k)
    override __.SyncTo t = target'.SyncTo t.Target
    member __.SlowCap (* TrafficStats *) : IVarProperty<Duration> = slowCap
    member __.TotalCount (* TrafficStats *) : IVarProperty<int> = totalCount
    member __.SlowCount (* TrafficStats *) : IVarProperty<int> = slowCount
    member __.PendingCount (* TrafficStats *) : IVarProperty<int> = pendingCount
    member __.SucceedCount (* TrafficStats *) : IVarProperty<int> = succeedCount
    member __.FailedCount (* TrafficStats *) : IVarProperty<int> = failedCount
    member __.FailedPkts (* TrafficStats *) : IListProperty<IVarProperty<PktLog>> = failedPkts

(*
 * Generated: <Record>
 *     IsJson, IsLoose
 *)
type StatusLog = {
    Time : (* StatusLog *) Instant
    Status : (* StatusLog *) LinkStatus
} with
    static member Create
        (
            ?time : (* StatusLog *) Instant,
            ?status : (* StatusLog *) LinkStatus
        ) : StatusLog =
        {
            Time = (* StatusLog *) time
                |> Option.defaultWith (fun () -> (getNow' ()))
            Status = (* StatusLog *) status
                |> Option.defaultWith (fun () -> LinkStatus.Unknown)
        }
    static member SetTime ((* StatusLog *) time : Instant) (this : StatusLog) =
        {this with Time = time}
    static member SetStatus ((* StatusLog *) status : LinkStatus) (this : StatusLog) =
        {this with Status = status}
    static member JsonEncoder : JsonEncoder<StatusLog> =
        fun (this : StatusLog) ->
            E.object [
                yield "time", E.instant (* StatusLog *) this.Time
                yield "status", LinkStatus.JsonEncoder (* StatusLog *) this.Status
            ]
    static member JsonDecoder : JsonDecoder<StatusLog> =
        D.object (fun get ->
            {
                Time = get.Optional.Field (* StatusLog *) "time" D.instant
                    |> Option.defaultValue (getNow' ())
                Status = get.Optional.Field (* StatusLog *) "status" LinkStatus.JsonDecoder
                    |> Option.defaultValue LinkStatus.Unknown
            }
        )
    static member JsonSpec =
        FieldSpec.Create<StatusLog> (StatusLog.JsonEncoder, StatusLog.JsonDecoder)
    interface IJson with
        member this.ToJson () = StatusLog.JsonEncoder this
    interface IObj
    member this.WithTime ((* StatusLog *) time : Instant) =
        this |> StatusLog.SetTime time
    member this.WithStatus ((* StatusLog *) status : LinkStatus) =
        this |> StatusLog.SetStatus status

(*
 * Generated: <Combo>
 *     IsFinal
 *)
type LinkStats (owner : IOwner, key : Key) =
    inherit WrapProperties<LinkStats, IComboProperty> ()
    let target' = Properties.combo (owner, key)
    let status = target'.AddVar<(* LinkStats *) StatusLog> (StatusLog.JsonEncoder, StatusLog.JsonDecoder, "status", (StatusLog.Create ()), None)
    let statusHistory = target'.AddList<(* LinkStats *) StatusLog> (StatusLog.JsonEncoder, StatusLog.JsonDecoder, "status_history", (StatusLog.Create ()), None)
    let send = target'.AddCustom<(* LinkStats *) TrafficStats> (TrafficStats.Create, "send")
    let receive = target'.AddCustom<(* LinkStats *) TrafficStats> (TrafficStats.Create, "receive")
    do (
        target'.SealCombo ()
        base.Setup (target')
    )
    static member Create (o, k) = new LinkStats (o, k)
    static member Create () = LinkStats.Create (noOwner, NoKey)
    static member AddToCombo key (combo : IComboProperty) =
        combo.AddCustom<LinkStats> (LinkStats.Create, key)
    override this.Self = this
    override __.Spawn (o, k) = LinkStats.Create (o, k)
    override __.SyncTo t = target'.SyncTo t.Target
    member __.Status (* LinkStats *) : IVarProperty<StatusLog> = status
    member __.StatusHistory (* LinkStats *) : IListProperty<IVarProperty<StatusLog>> = statusHistory
    member __.Send (* LinkStats *) : TrafficStats = send
    member __.Receive (* LinkStats *) : TrafficStats = receive
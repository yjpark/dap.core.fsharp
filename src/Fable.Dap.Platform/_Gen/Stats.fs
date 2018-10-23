[<AutoOpen>]
module Dap.Platform.Stats

open Dap.Prelude
open Dap.Context

(*
 * Generated: <Record>
 *     IsJson, IsLoose
 *)
type OpLog = {
    Op : (* OpLog *) string
    Msg : (* OpLog *) string
    Time : (* OpLog *) Instant
    Duration : (* OpLog *) Duration
    StackTrace : (* OpLog *) string
} with
    static member Create
        (
            ?op : string,
            ?msg : string,
            ?time : Instant,
            ?duration : Duration,
            ?stackTrace : string
        ) : OpLog =
        {
            Op = (* OpLog *) op
                |> Option.defaultWith (fun () -> "")
            Msg = (* OpLog *) msg
                |> Option.defaultWith (fun () -> "")
            Time = (* OpLog *) time
                |> Option.defaultWith (fun () -> (getNow' ()))
            Duration = (* OpLog *) duration
                |> Option.defaultWith (fun () -> noDuration)
            StackTrace = (* OpLog *) stackTrace
                |> Option.defaultWith (fun () -> "")
        }
    static member Default () =
        OpLog.Create (
            "", (* OpLog *) (* op *)
            "", (* OpLog *) (* msg *)
            (getNow' ()), (* OpLog *) (* time *)
            noDuration, (* OpLog *) (* duration *)
            "" (* OpLog *) (* stackTrace *)
        )
    static member SetOp ((* OpLog *) op : string) (this : OpLog) =
        {this with Op = op}
    static member SetMsg ((* OpLog *) msg : string) (this : OpLog) =
        {this with Msg = msg}
    static member SetTime ((* OpLog *) time : Instant) (this : OpLog) =
        {this with Time = time}
    static member SetDuration ((* OpLog *) duration : Duration) (this : OpLog) =
        {this with Duration = duration}
    static member SetStackTrace ((* OpLog *) stackTrace : string) (this : OpLog) =
        {this with StackTrace = stackTrace}
    static member JsonEncoder : JsonEncoder<OpLog> =
        fun (this : OpLog) ->
            E.object [
                "op", E.string (* OpLog *) this.Op
                "msg", E.string (* OpLog *) this.Msg
                "time", E.instant (* OpLog *) this.Time
                "duration", E.duration (* OpLog *) this.Duration
                "stack_trace", E.string (* OpLog *) this.StackTrace
            ]
    static member JsonDecoder : JsonDecoder<OpLog> =
        D.object (fun get ->
            {
                Op = get.Optional.Field (* OpLog *) "op" D.string
                    |> Option.defaultValue ""
                Msg = get.Optional.Field (* OpLog *) "msg" D.string
                    |> Option.defaultValue ""
                Time = get.Optional.Field (* OpLog *) "time" D.instant
                    |> Option.defaultValue (getNow' ())
                Duration = get.Optional.Field (* OpLog *) "duration" D.duration
                    |> Option.defaultValue noDuration
                StackTrace = get.Optional.Field (* OpLog *) "stack_trace" D.string
                    |> Option.defaultValue ""
            }
        )
    static member JsonSpec =
        FieldSpec.Create<OpLog> (OpLog.JsonEncoder, OpLog.JsonDecoder)
    interface IJson with
        member this.ToJson () = OpLog.JsonEncoder this
    interface IObj
    member this.WithOp ((* OpLog *) op : string) =
        this |> OpLog.SetOp op
    member this.WithMsg ((* OpLog *) msg : string) =
        this |> OpLog.SetMsg msg
    member this.WithTime ((* OpLog *) time : Instant) =
        this |> OpLog.SetTime time
    member this.WithDuration ((* OpLog *) duration : Duration) =
        this |> OpLog.SetDuration duration
    member this.WithStackTrace ((* OpLog *) stackTrace : string) =
        this |> OpLog.SetStackTrace stackTrace

(*
 * Generated: <Class>
 *     IsFinal
 *)
type DurationStats (owner : IOwner, key : Key) =
    inherit WrapProperties<DurationStats, IComboProperty> ()
    let target' = Properties.combo (owner, key)
    let slowCap = target'.AddVar<(* DurationStats *) Duration> (E.duration, D.duration, "slow_cap", noDuration, None)
    let totalCount = target'.AddVar<(* DurationStats *) int> (E.int, D.int, "total_count", 0, None)
    let slowCount = target'.AddVar<(* DurationStats *) int> (E.int, D.int, "slow_count", 0, None)
    do (
        target'.SealCombo ()
        base.Setup (target')
    )
    static member Create (o, k) = new DurationStats (o, k)
    static member Default () = DurationStats.Create (noOwner, NoKey)
    static member AddToCombo key (combo : IComboProperty) =
        combo.AddCustom<DurationStats> (DurationStats.Create, key)
    override this.Self = this
    override __.Spawn (o, k) = DurationStats.Create (o, k)
    override __.SyncTo t = target'.SyncTo t.Target
    member __.SlowCap (* DurationStats *) : IVarProperty<Duration> = slowCap
    member __.TotalCount (* DurationStats *) : IVarProperty<int> = totalCount
    member __.SlowCount (* DurationStats *) : IVarProperty<int> = slowCount

(*
 * Generated: <Class>
 *     IsFinal
 *)
type FuncStats (owner : IOwner, key : Key) =
    inherit WrapProperties<FuncStats, IComboProperty> ()
    let target' = Properties.combo (owner, key)
    let slowCap = target'.AddVar<(* DurationStats *) Duration> (E.duration, D.duration, "slow_cap", noDuration, None)
    let totalCount = target'.AddVar<(* DurationStats *) int> (E.int, D.int, "total_count", 0, None)
    let slowCount = target'.AddVar<(* DurationStats *) int> (E.int, D.int, "slow_count", 0, None)
    let pendingCount = target'.AddVar<(* FuncStats *) int> (E.int, D.int, "pending_count", 0, None)
    let succeedCount = target'.AddVar<(* FuncStats *) int> (E.int, D.int, "succeed_count", 0, None)
    let failedCount = target'.AddVar<(* FuncStats *) int> (E.int, D.int, "failed_count", 0, None)
    let failedOps = target'.AddList<(* FuncStats *) OpLog> (OpLog.JsonEncoder, OpLog.JsonDecoder, "failed_ops", (OpLog.Default ()), None)
    do (
        target'.SealCombo ()
        base.Setup (target')
    )
    static member Create (o, k) = new FuncStats (o, k)
    static member Default () = FuncStats.Create (noOwner, NoKey)
    static member AddToCombo key (combo : IComboProperty) =
        combo.AddCustom<FuncStats> (FuncStats.Create, key)
    override this.Self = this
    override __.Spawn (o, k) = FuncStats.Create (o, k)
    override __.SyncTo t = target'.SyncTo t.Target
    member __.SlowCap (* DurationStats *) : IVarProperty<Duration> = slowCap
    member __.TotalCount (* DurationStats *) : IVarProperty<int> = totalCount
    member __.SlowCount (* DurationStats *) : IVarProperty<int> = slowCount
    member __.PendingCount (* FuncStats *) : IVarProperty<int> = pendingCount
    member __.SucceedCount (* FuncStats *) : IVarProperty<int> = succeedCount
    member __.FailedCount (* FuncStats *) : IVarProperty<int> = failedCount
    member __.FailedOps (* FuncStats *) : IListProperty<IVarProperty<OpLog>> = failedOps

(*
 * Generated: <Class>
 *)
type Stats (owner : IOwner, key : Key) =
    inherit WrapProperties<Stats, IComboProperty> ()
    let target' = Properties.combo (owner, key)
    let time = target'.AddVar<(* Stats *) Instant> (E.instant, D.instant, "time", (getNow' ()), None)
    let deliver = target'.AddCustom<(* Stats *) DurationStats> (DurationStats.Create, "deliver")
    let process' = target'.AddCustom<(* Stats *) DurationStats> (DurationStats.Create, "process")
    let reply = target'.AddCustom<(* Stats *) FuncStats> (FuncStats.Create, "reply")
    let func = target'.AddCustom<(* Stats *) FuncStats> (FuncStats.Create, "func")
    let task = target'.AddCustom<(* Stats *) FuncStats> (FuncStats.Create, "task")
    do (
        base.Setup (target')
    )
    static member Create (o, k) = new Stats (o, k)
    static member Default () = Stats.Create (noOwner, NoKey)
    static member AddToCombo key (combo : IComboProperty) =
        combo.AddCustom<Stats> (Stats.Create, key)
    override this.Self = this
    override __.Spawn (o, k) = Stats.Create (o, k)
    override __.SyncTo t = target'.SyncTo t.Target
    member __.Time (* Stats *) : IVarProperty<Instant> = time
    member __.Deliver (* Stats *) : DurationStats = deliver
    member __.Process (* Stats *) : DurationStats = process'
    member __.Reply (* Stats *) : FuncStats = reply
    member __.Func (* Stats *) : FuncStats = func
    member __.Task (* Stats *) : FuncStats = task
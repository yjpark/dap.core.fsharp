[<AutoOpen>]
module Dap.Platform.Runnable

open Dap.Prelude

type IRunnable<'initer, 'runner, 'args, 'model, 'msg>
                                when 'initer :> IRunner and 'runner :> IRunner and 'msg :> IMsg =
    inherit IRunner
    inherit IDispatcher<'msg>
    abstract Args : 'args with get
    abstract Logic : Logic<'initer, 'runner, 'args, 'model, 'msg> with get
    abstract State : 'model option with get
    abstract Start : unit -> Cmd<'msg>
    abstract Process : 'msg -> Cmd<'msg>
    abstract Deliver : Cmd<'msg> -> unit
    abstract Initer : 'initer
    abstract Runner : 'runner

let private tplRunnableFailed = LogEvent.Template2WithException<string, obj>(LogLevelError, "[{Section}] {Msg} -> Failed")

let private tplSlowStats = LogEvent.Template5<string, Duration, IMsg, string, string>(LogLevelInformation, "[{Section}] {Duration}<ms> {Msg} ~> {Detail}\n{StackTrace}")

let internal start' (runnable : IRunnable<'initer, 'runner, 'args, 'model, 'msg>)
                (setState : 'model -> unit) : Cmd<'msg> =
    let runner = runnable.Initer
    try
        let (model, cmd) =
            match runnable.State with
            | None ->
                runnable.Logic.Init runner runnable.Args
            | Some state ->
                failWith "Already_Started" state
        setState model
        (runner :> ITaskManager).StartPendingTasks () |> ignore
        let runner = runnable.Runner
        try
            batchCmd [
                cmd
                runnable.Logic.Subscribe runner model
            ]
        with e ->
            runner.Log <| tplRunnableFailed "Subscribe" runnable.Args e
            noCmd
    with e ->
        runner.Log <| tplRunnableFailed "Init" runnable.Args e
        noCmd

let private getSlowProcessMessage (msg : IMsg) (duration, stats) =
    tplSlowStats "Slow_Process" duration msg stats

let trackDeliverDuration (runnable : IRunnable<'initer, 'runner, 'args, 'model, 'msg>)
        (platform : string)
        (fromTime : Instant)
        (msg : 'msg) : unit =
    let stats = runnable.Dash0.Stats.Deliver
    let (duration, slowOpLog) = stats.AddOp runnable platform fromTime
    slowOpLog
    |> Option.iter (fun opLog ->
        runnable.Log <| tplSlowStats "Slow_Deliver" duration msg (stats.ToLogDetail ()) opLog.StackTrace
    )
let internal process' (runnable : IRunnable<'initer, 'runner, 'args, 'model, 'msg>)
                (msg : 'msg)
                (setState : 'model -> unit)
                : Cmd<'msg> =
    let runner = runnable.Runner
    try
        let time = runner.Clock.Now'
        let (model, cmd) =
            match runnable.State with
            | None ->
                failWith "Not_Started" msg
            | Some state ->
                runnable.Logic.Update runner msg state
        setState model
        (runner :> ITaskManager).StartPendingTasks () |> ignore
        let stats = runnable.Dash0.Stats.Process
        let (duration, slowOpLog) = stats.AddOp runnable "" time
        slowOpLog
        |> Option.iter (fun opLog ->
            runnable.Log <| tplSlowStats "Slow_Process" duration msg (stats.ToLogDetail ()) opLog.StackTrace
        )
        cmd
    with e ->
        runner.Log <| tplRunnableFailed "Update" msg e
        noCmd

let internal deliver' (runnable : IRunnable<'initer, 'runner, 'args, 'model, 'msg>)
                (cmd : Cmd<'msg>) : unit =
    cmd |> List.iter (fun m -> m <| dispatch' runnable)

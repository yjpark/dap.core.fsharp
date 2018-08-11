[<AutoOpen>]
module Dap.Platform.Runnable

open Elmish
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

let private tplSlowStats = LogEvent.Template4<string, float<ms>, IMsg, DurationStats<ms>>(LogLevelWarning, "[{Section}] {Duration}<ms> {Msg} ~> {Detail}")

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
            Cmd.batch [
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

let private getSlowDeliverMessage (msg : IMsg) (duration, stats) =
    tplSlowStats "Slow_Deliver" duration msg stats

let internal trackDeliverDuration (runnable : IRunnable<'initer, 'runner, 'args, 'model, 'msg>) fromTime msg : unit =
    trackDurationStatsInMs runnable fromTime runnable.Stats.Deliver (getSlowDeliverMessage msg) |> ignore

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
        trackDurationStatsInMs runner time runnable.Stats.Process (getSlowProcessMessage msg) |> ignore
        cmd
    with e ->
        runner.Log <| tplRunnableFailed "Update" msg e
        noCmd

let internal deliver' (runnable : IRunnable<'initer, 'runner, 'args, 'model, 'msg>)
                (cmd : Cmd<'msg>) : unit =
    cmd |> List.iter (fun m -> m <| dispatch' runnable)

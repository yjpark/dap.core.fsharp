[<AutoOpen>]
module Dap.Platform.Runnable

open Elmish
open Dap.Prelude

type IRunnable<'runner, 'args, 'model, 'msg>
                                when 'runner :> IRunner and 'msg :> IMsg =
    inherit IRunner<'runner>
    inherit IDispatcher<'msg>
    abstract Args : 'args with get
    abstract Logic : Logic<'runner, 'args, 'model, 'msg> with get
    abstract State : 'model option with get
    abstract Start : unit -> Cmd<'msg>
    abstract Process : 'msg -> Cmd<'msg>
    abstract Deliver : Cmd<'msg> -> unit

let private tplRunnableErr = LogEvent.Template3<string, obj, obj>(LogLevelFatal, "[{Section}] {Err}: {Detail}")

let private tplSlowStats = LogEvent.Template4<string, float<ms>, IMsg, DurationStats<ms>>(LogLevelWarning, "[{Section}] {Duration}<ms> {Msg} ~> {Detail}")

let private raiseRunnableErr err detail = 
    raiseWith <| tplRunnableErr "Runnable" err detail

let internal start' (runnable : IRunnable<'runner, 'args, 'model, 'msg>)
                (setState : 'model -> unit) : Cmd<'msg> =
    let runner = runnable.Self
    let (model, cmd) =
        match runnable.State with
        | None ->
            runnable.Logic.Init runner runnable.Args
        | Some state ->
            raiseRunnableErr "Already_Started" state
    setState model
    Cmd.batch [
        cmd
        runnable.Logic.Subscribe runner model
    ]

let private getSlowProcessMessage (msg : IMsg) (duration, stats) =
    tplSlowStats "Slow_Process" duration msg stats

let private getSlowDeliverMessage (msg : IMsg) (duration, stats) =
    tplSlowStats "Slow_Deliver" duration msg stats

let internal trackDeliverDuration (runnable : IRunnable<'runner, 'args, 'model, 'msg>) fromTime msg : unit =
    trackDurationStatsInMs runnable fromTime runnable.Stats.Deliver (getSlowDeliverMessage msg) |> ignore

let internal process' (runnable : IRunnable<'runner, 'args, 'model, 'msg>)
                (msg : 'msg)
                (setState : 'model -> unit)
                : Cmd<'msg> =
    let runner = runnable.Self
    let time = runner.Clock.Now'
    let (model, cmd) =
        match runnable.State with
        | None ->
            raiseRunnableErr "Not_Started" msg
        | Some state ->
            runnable.Logic.Update runner state msg
    setState model
    trackDurationStatsInMs runner time runnable.Stats.Process (getSlowProcessMessage msg) |> ignore
    cmd

let internal deliver' (runnable : IRunnable<'runner, 'args, 'model, 'msg>)
                (cmd : Cmd<'msg>) : unit =
    cmd |> List.iter (fun m -> m <| dispatch' runnable)

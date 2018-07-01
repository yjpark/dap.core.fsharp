[<AutoOpen>]
[<RequireQualifiedAccess>]
module Dap.Platform.Ticker.Logic

open Elmish
open NodaTime
open Dap.Prelude
open Dap.Platform

type private ActorOperate = ActorOperate<Args, Model, Msg, Req, Evt>

let private doStartTimer req (callback : Callback<Instant>) : ActorOperate =
    fun runner (model, cmd) ->
        match model.Timer with
        | None ->
            let interval = 1000.0 / runner.Actor.Args.FrameRate
            if (interval > 0.0) then
                let timer = new System.Timers.Timer(Interval = interval, Enabled = true, AutoReset = true)
                timer.Elapsed.AddHandler(new System.Timers.ElapsedEventHandler(fun _src _evt ->
                    runner.Deliver <| InternalEvt DoTick
                ))
                let now = runner.Clock.Now
                reply runner callback <| ack req now
                ({model with
                    BeginTime = Some now
                    FinishTime = None
                    Ticking = false
                    FrameIndex = 0
                    DroppedCount = 0
                    LastTickTime = runner.Clock.Now
                    LastTickStats = noTickStats
                    LastLateTickStats = noTickStats
                    Timer = Some timer
                }, cmd)
            else
                reply runner callback <| nak req "Invalid_FrameRate" runner.Actor.Args
                (model, cmd)
        | Some _timer ->
            reply runner callback <| nak req "Already_Started" model
            (model, cmd)

let private doStopTimer req (callback : Callback<Instant>) : ActorOperate =
    fun runner (model, cmd) ->
        match model.Timer with
        | Some timer ->
            timer.Stop ()
            let now = runner.Clock.Now
            reply runner callback <| ack req now
            ({model with
                FinishTime = Some now
                Timer = None
            }, cmd)
        | None ->
            reply runner callback <| nak req "Not_Started" model
            (model, cmd)

let private handleReq req : ActorOperate =
    fun runner (model, cmd) ->
        match req with
        | DoStartTimer a -> doStartTimer req a
        | DoStopTimer a -> doStopTimer req a
        <| runner <| (model, cmd)

let private doTick : ActorOperate =
    fun runner (model, cmd) ->
        match model.Ticking with
        | true ->
            logError runner "Ticker" "Frame_Dropped" model
            (runner, model, cmd)
            |=|> setModel {model with DroppedCount = model.DroppedCount + 1}
        | false ->
            let time' = runner.Clock.Now'
            let time = runner.Clock.Now
            let delta = time - model.LastTickTime
            (runner, model, cmd)
            |-|> setModel
                {model with
                    Ticking = true
                    FrameIndex = model.FrameIndex + 1
                    LastTickTime = time
                }
            |-|> addCmd ^<| TickerEvt ^<| OnTick (time, delta)
            |=|> addCmd ^<| InternalEvt ^<| OnTickDone (time', time, delta)

let private onTickDone ((time', time, delta) : Instant * Instant * Duration) : ActorOperate =
    fun runner (model, cmd) ->
        let (doneTime', duration) = runner.Clock.CalcDuration' (time')
        let stats = {
            Time' = time'
            Time = time
            Delta = delta
            Duration = duration
        }
        (runner, model, cmd)
        |-|> setModel {model with LastTickStats = stats}
        |-|> addCmd ^<| TickerEvt ^<| OnTick' stats
        |-|> addCmd ^<| TickerEvt ^<| OnLateTick (time, delta)
        |=|> addCmd ^<| InternalEvt ^<| OnLateTickDone (doneTime', time, delta)

let private onLateTickDone ((time', time, delta) : Instant * Instant * Duration) : ActorOperate =
    fun runner (model, cmd) ->
        let (_doneTime', duration) = runner.Clock.CalcDuration' (time')
        let stats = {
            Time' = time'
            Time = time
            Delta = delta
            Duration = duration
        }
        (runner, model, cmd)
        |-|> setModel
            {model with
                LastLateTickStats = stats
                Ticking = false
            }
        |-|> addCmd ^<| TickerEvt ^<| OnLateTick' stats
        |=|> addCmd ^<| TickerEvt ^<| OnLateTick (time, delta)

let private handleInternalEvt evt : ActorOperate =
    fun runner (model, cmd) ->
        match evt with
        | DoTick -> doTick
        | OnTickDone (a, b, c) -> onTickDone (a, b, c)
        | OnLateTickDone (a, b, c) -> onLateTickDone (a, b, c)
        <| runner <| (model, cmd)

let private update : ActorUpdate<Args, Model, Msg, Req, Evt> =
    fun runner model msg ->
        match msg with
        | InternalEvt evt -> handleInternalEvt evt
        | TickerReq req -> handleReq req
        | TickerEvt _evt -> noOperation
        <| runner <| (model, [])

let private init : ActorInit<Args, Model, Msg, Req, Evt> =
    fun runner args ->
        let cmd =
            if args.AutoStart then
                Cmd.ofMsg <| TickerReq ^<| DoStartTimer None
            else
                noCmd
        ({
            BeginTime = None
            FinishTime = None
            Ticking = false
            FrameIndex = 0
            DroppedCount = 0
            LastTickTime = runner.Clock.Now
            LastTickStats = noTickStats
            LastLateTickStats = noTickStats
            Timer = None
        }, cmd)

let logic : ActorLogic<Args, Model, Msg, Req, Evt> =
    {
        Init = init
        Update = update
        Subscribe = noSubscription
    }

let getSpec (newArgs : ActorNewArgs<Args>) : AgentSpec<Args, Model, Msg, Req, Evt> =
    {
        Actor =
            {
                NewArgs = newArgs
                Logic = logic
                WrapReq = TickerReq
                CastEvt = castEvt
            }
        OnAgentEvent = None
        GetSlowCap = None
    }

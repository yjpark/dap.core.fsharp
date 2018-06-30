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
        match model.State with
        | None ->
            let interval = 1000.0 / runner.Actor.Args.FrameRate
            if (interval > 0.0) then
                let timer = new System.Timers.Timer(Interval = interval, Enabled = true, AutoReset = true)
                timer.Elapsed.AddHandler(new System.Timers.ElapsedEventHandler(fun _src _evt ->
                    runner.Actor.Args.FireInternalEvent' DoTick
                ))
                let state = {
                    Timer = timer
                }
                let now = runner.Clock.Now
                reply runner callback <| ack req now
                ({model with
                    BeginTime = Some now
                    FinishTime = None
                    LastTickTime = now
                    LastTickStats = noTickStats
                    LastLateTickStats = noTickStats
                    State = Some state
                }, cmd)
            else
                reply runner callback <| nak req "Invalid_FrameRame" runner.Actor.Args.FrameRate
                (model, cmd)
        | Some state ->
            reply runner callback <| nak req "Already_Started" model
            (model, cmd)

let private doStopTimer req (callback : Callback<Instant>) : ActorOperate =
    fun runner (model, cmd) ->
        match model.State with
        | Some state ->
            state.Timer.Stop ()
            let now = runner.Clock.Now
            reply runner callback <| ack req now
            ({model with
                FinishTime = Some now
                State = None
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
        let time = runner.Clock.Now
        let time' = runner.Clock.Now'
        let delta = time - model.LastTickTime
        runner.Actor.Args.FireEvent' <| OnWillTick time
        runner.Actor.Args.FireEvent' <| OnTick (time, delta)
        let (time', duration) = runner.Clock.CalcDuration' (time')
        let stats = {
            Time = time
            Delta = delta
            Duration = duration
        }
        runner.Actor.Args.FireEvent' <| OnTick' stats
        runner.Actor.Args.FireEvent' <| OnLateTick (time, delta)
        let (time', duration) = runner.Clock.CalcDuration' (time')
        let stats = {stats with Duration = duration}
        runner.Actor.Args.FireEvent' <| OnLateTick' stats
        (model, cmd)

let private handleInternalEvt evt : ActorOperate =
    fun runner (model, cmd) ->
        match evt with
        | DoTick ->
            doTick
        <| runner <| (model, cmd)

let private handleEvt evt : ActorOperate =
    fun runner (model, cmd) ->
        match evt with
        | OnWillTick time ->
            setModel {model with FrameIndex = model.FrameIndex + 1 ; LastTickTime = time}
        | OnTick' stats ->
            setModel {model with LastTickStats = stats}
        | OnLateTick' stats ->
            setModel {model with LastLateTickStats = stats}
        | _ -> noOperation
        <| runner <| (model, cmd)

let private update : ActorUpdate<Args, Model, Msg, Req, Evt> =
    fun runner model msg ->
        match msg with
        | InternalEvt evt -> handleInternalEvt evt
        | TickerEvt evt -> handleEvt evt
        | TickerReq req -> handleReq req
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
            FrameIndex = 0
            LastTickTime = Instant.MinValue
            LastTickStats = noTickStats
            LastLateTickStats = noTickStats
            State = None
        }, cmd)

let private subscribe : ActorSubscribe<Args, Model, Msg, Req, Evt> =
    fun runner model ->
        Cmd.batch [
            subscribeEvent runner model InternalEvt runner.Actor.Args.OnInternalEvent
            subscribeEvent runner model TickerEvt runner.Actor.Args.OnEvent
        ]

let logic : ActorLogic<Args, Model, Msg, Req, Evt> =
    {
        Init = init
        Update = update
        Subscribe = subscribe
    }

let getSpec (newArgs : NewArgs<Args>) : AgentSpec<Args, Model, Msg, Req, Evt> =
    {
        Actor =
            {
                NewArgs = newArgs
                Logic = logic
                WrapReq = TickerReq
                GetOnEvent = fun args -> args.OnEvent
            }
        OnAgentEvent = None
        GetSlowCap = None
    }

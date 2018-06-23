[<AutoOpen>]
[<RequireQualifiedAccess>]
module Dap.Platform.Ticker.Logic

open Elmish
open NodaTime
open Dap.Prelude
open Dap.Platform

let private doStartTimer msg (callback : Callback<Instant>) : Operate<IRunner, Model, Msg> =
    fun runner (model, cmd) ->
        match model.State with
        | None ->
            let interval = 1000.0 / double (model.Args.FrameRate)
            if (interval > 0.0) then
                let timer = new System.Timers.Timer(Interval = interval, Enabled = true, AutoReset = true)
                timer.Elapsed.AddHandler(new System.Timers.ElapsedEventHandler(fun _src _evt ->
                    model.Args.FireInternalEvent' DoTick
                ))
                let state = {
                    Args = model.Args
                    Timer = timer
                }
                let now = runner.Clock.Now
                reply runner callback <| ack msg now
                ({model with
                    BeginTime = Some now
                    FinishTime = None
                    LastTickTime = now
                    LastTickStats = noTickStats
                    LastLateTickStats = noTickStats
                    State = Some state
                }, cmd)
            else
                reply runner callback <| nak msg "Invalid_FrameRame" model.Args.FrameRate
                (model, cmd)
        | Some state ->
            reply runner callback <| nak msg "Already_Started" model
            (model, cmd)

let private doStopTimer msg (callback : Callback<Instant>) : Operate<IRunner, Model, Msg> =
    fun runner (model, cmd) ->
        match model.State with
        | Some state ->
            state.Timer.Stop ()
            let now = runner.Clock.Now
            reply runner callback <| ack msg now
            ({model with
                FinishTime = Some now
                State = None
            }, cmd)
        | None ->
            reply runner callback <| nak msg "Not_Started" model
            (model, cmd)

let private handleReq msg req : Operate<IRunner, Model, Msg> =
    fun runner (model, cmd) ->
        match req with
        | DoStartTimer a -> doStartTimer msg a
        | DoStopTimer a -> doStopTimer msg a
        <| runner <| (model, cmd)

let private doTick : Operate<IRunner, Model, Msg> =
    fun runner (model, cmd) ->
        let time = runner.Clock.Now
        let time' = runner.Clock.Now'
        let delta = time - model.LastTickTime
        model.Args.FireEvent' <| OnWillTick time
        model.Args.FireEvent' <| OnTick (time, delta)
        let (time', duration) = runner.Clock.CalcDuration' (time')
        let stats = {
            Time = time
            Delta = delta
            Duration = duration
        }
        model.Args.FireEvent' <| OnTick' stats
        model.Args.FireEvent' <| OnLateTick (time, delta)
        let (time', duration) = runner.Clock.CalcDuration' (time')
        let stats = {stats with Duration = duration}
        model.Args.FireEvent' <| OnLateTick' stats
        (model, cmd)

let private handleInternalEvt msg evt : Operate<IRunner, Model, Msg> =
    fun runner (model, cmd) ->
        match evt with
        | DoTick ->
            doTick
        <| runner <| (model, cmd)

let private handleEvt msg evt : Operate<IRunner, Model, Msg> =
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

let private update : Update<IRunner, Model, Msg> =
    fun runner model msg -> 
        match msg with
        | InternalEvt evt -> handleInternalEvt msg evt
        | TickerEvt evt -> handleEvt msg evt
        | TickerReq req -> handleReq msg req
        <| runner <| (model, [])

let private init : Init<IAgent, Args, Model, Msg> =
    fun runner args ->
        let cmd =
            if args.AutoStart then
                Cmd.ofMsg <| TickerReq ^<| DoStartTimer None
            else
                noCmd
        ({
            Args = args
            BeginTime = None
            FinishTime = None
            FrameIndex = 0
            LastTickTime = Instant.MinValue
            LastTickStats = noTickStats
            LastLateTickStats = noTickStats
            State = None
        }, cmd)

let private subscribe : Subscribe<IAgent, Model, Msg> =
    fun runner model ->
        Cmd.batch [
            subscribeEvent runner model InternalEvt model.Args.OnInternalEvent
            subscribeEvent runner model TickerEvt model.Args.OnEvent
        ]

let logic : Logic<IAgent, Args, Model, Msg> =
    {
        Init = init
        Update = update
        Subscribe = subscribe
    }

let getSpec (newArgs : IOwner -> Args) : AgentSpec<Args, Model, Msg, Req, Evt> =
    {
        Actor =
            {
                NewArgs = newArgs
                Logic = logic
                WrapReq = TickerReq
                GetOnEvent = fun model -> model.Args.OnEvent
            }
        OnAgentEvent = None
        GetSlowCap = None
    }

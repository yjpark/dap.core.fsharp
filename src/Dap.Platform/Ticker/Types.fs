module Dap.Platform.Ticker.Types

open NodaTime
open Dap.Prelude
open Dap.Platform

type TickStats = {
    Time' : Instant
    Time : Instant
    Delta : Duration
    Duration : Duration
}

and Args = {
    FrameRate : double
    AutoStart : bool
} with
    static member New (frameRate, autoStart) =
        {
            FrameRate = frameRate
            AutoStart = autoStart
        }
    static member New (frameRate) = Args.New (frameRate, true)

and Model = {
    BeginTime : Instant option
    FinishTime : Instant option
    Ticking : bool
    FrameIndex : int
    DroppedCount : int
    LastTickTime : Instant
    LastTickStats : TickStats
    LastLateTickStats : TickStats
    Timer : System.Timers.Timer option
}

and Req =
    | DoStartTimer of Callback<Instant>
    | DoStopTimer of Callback<Instant>
with interface IReq

and Evt =
    | OnTick of Instant * Duration        // delta
    | OnTick' of TickStats
    | OnLateTick of Instant * Duration    // delta
    | OnLateTick' of TickStats
with interface IEvt

and InternalEvt =
    | DoTick
    | OnTickDone of Instant * Instant * Duration
    | OnLateTickDone of Instant * Instant * Duration

and Msg =
    | InternalEvt of InternalEvt
    | TickerReq of Req
    | TickerEvt of Evt
with interface IMsg

let castEvt : CastEvt<Msg, Evt> =
    function
    | TickerEvt evt -> Some evt
    | _ -> None

let noTickStats = {
    Time' = Instant.MinValue
    Time = Instant.MinValue
    Delta = Duration.FromSeconds 0L
    Duration = Duration.FromSeconds 0L
}

type Agent (param) =
    inherit BaseAgent<Agent, Args, Model, Msg, Req, Evt> (param)
    override this.Runner = this
    static member Spawn (param) = new Agent (param)

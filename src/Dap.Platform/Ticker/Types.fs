[<AutoOpen>]
[<RequireQualifiedAccess>]
module Dap.Platform.Ticker.Types

open NodaTime
open Dap.Prelude
open Dap.Platform

type TickStats = {
    Time : Instant
    Delta : Duration
    Duration : Duration
}

type Args = {
    AutoStart : bool
    FrameRate : double
    Event' : Bus<Evt>
    InternalEvent' : Bus<InternalEvt>
} with
    member this.FireEvent' = this.Event'.Trigger
    member this.OnEvent = this.Event'.Publish
    member this.FireInternalEvent' = this.InternalEvent'.Trigger
    member this.OnInternalEvent = this.InternalEvent'.Publish

and State = {
    Timer : System.Timers.Timer
}

and Model = {
    BeginTime : Instant option
    FinishTime : Instant option
    FrameIndex : int
    LastTickTime : Instant
    LastTickStats : TickStats
    LastLateTickStats : TickStats
    State : State option
}

and Req =
    | DoStartTimer of Callback<Instant>
    | DoStopTimer of Callback<Instant>
with interface IReq

and Evt =
    | OnWillTick of Instant
    | OnTick of Instant * Duration        // delta
    | OnTick' of TickStats
    | OnLateTick of Instant * Duration    // delta
    | OnLateTick' of TickStats
with interface IEvt

and InternalEvt =
    | DoTick

and Msg =
    | InternalEvt of InternalEvt
    | TickerReq of Req
    | TickerEvt of Evt
with interface IMsg

let noTickStats = {
    Time = Instant.MinValue
    Delta = Duration.FromSeconds 0L
    Duration = Duration.FromSeconds 0L
}
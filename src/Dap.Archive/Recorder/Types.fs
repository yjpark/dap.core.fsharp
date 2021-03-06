module Dap.Archive.Recorder.Types

open System.IO

open Dap.Prelude
open Dap.Context
open Dap.Platform
open Dap.Remote
open Dap.Archive

module TickerTypes = Dap.Platform.Ticker.Types

type Args = RecorderArgs

and Model<'extra, 'frame> when 'extra :> IJson and 'frame :> IFrame = {
    Bundle : Bundle'<'extra, 'frame> option
    NextFlushTime : Instant
}

and Req<'extra, 'frame> when 'extra :> IJson and 'frame :> IFrame =
    | DoBeginRecording of Bundle'<'extra, 'frame> * Callback<Meta<'extra>>
    | DoFinishRecording of Callback<Meta<'extra>>
    | DoAppendFrame of 'frame * Callback<Meta<'extra> * 'frame>
with interface IReq

and Evt<'extra, 'frame> when 'extra :> IJson and 'frame :> IFrame =
    | OnBeginRecording of Meta<'extra>
    | OnFinishRecording of Meta<'extra>
    | OnAppendFrame of Meta<'extra> * 'frame
    | OnAppendFrameFailed of 'frame * exn
with interface IEvt

and Msg<'extra, 'frame> when 'extra :> IJson and 'frame :> IFrame =
    | RecorderReq of Req<'extra, 'frame>
    | RecorderEvt of Evt<'extra, 'frame>
    | OnTick of Instant * Duration
with interface IMsg

let castEvt<'extra, 'frame when 'extra :> IJson and 'frame :> IFrame> : CastEvt<Msg<'extra, 'frame>, Evt<'extra, 'frame>> =
    function
    | RecorderEvt evt -> Some evt
    | _ -> None

let DoBeginRecording (bundle : Bundle'<'extra, 'frame>)  callback =
    DoBeginRecording (bundle, callback)

let DoAppendFrame (frame : 'frame) callback =
    DoAppendFrame (frame, callback)

type Agent<'extra, 'frame> when 'extra :> IJson and 'frame :> IFrame (pack, param) =
    inherit PackAgent<ITickingPack, Agent<'extra, 'frame>, Args, Model<'extra, 'frame>, Msg<'extra, 'frame>, Req<'extra, 'frame>, Evt<'extra, 'frame>> (pack, param)
    override this.Runner = this
    static member Spawn k m = new Agent<'extra, 'frame> (k, m)


[<AutoOpen>]
module Dap.Archive.Recorder.Types

open System.IO
open Dap.Prelude
open Dap.Platform
open Dap.Remote
open Dap.Archive

type Args<'extra, 'frame> when 'extra :> JsonRecord and 'frame :> IFrame = {
    Event' : Bus<Evt<'extra, 'frame>> 
} with
    member this.FireEvent' = this.Event'.Trigger
    member this.OnEvent = this.Event'.Publish

and Model<'extra, 'frame> when 'extra :> JsonRecord and 'frame :> IFrame = {
    Bundle : Bundle'<'extra, 'frame> option
}

and Req<'extra, 'frame> when 'extra :> JsonRecord and 'frame :> IFrame =
    | DoBeginRecording of Bundle'<'extra, 'frame> * Callback<Meta<'extra>>
    | DoFinishRecording of Callback<Meta<'extra>>
    | DoAppendFrame of 'frame * Callback<Meta<'extra> * 'frame>
with interface IReq

and Evt<'extra, 'frame> when 'extra :> JsonRecord and 'frame :> IFrame =
    | OnBeginRecording of Meta<'extra>
    | OnFinishRecording of Meta<'extra>
    | OnAppendFrame of Meta<'extra> * 'frame
    | OnAppendFrameFailed of 'frame * exn
with interface IEvt

and Msg<'extra, 'frame> when 'extra :> JsonRecord and 'frame :> IFrame =
    | RecorderReq of Req<'extra, 'frame>
    | RecorderEvt of Evt<'extra, 'frame>
with interface IMsg

type Agent<'extra, 'frame> when 'extra :> JsonRecord and 'frame :> IFrame =
    IAgent<Args<'extra, 'frame>, Model<'extra, 'frame>, Req<'extra, 'frame>, Evt<'extra, 'frame>>

let DoBeginRecording' (bundle : Bundle'<'extra, 'frame>)  callback =
    DoBeginRecording (bundle, callback)

let DoAppendFrame' (frame : 'frame) callback =
    DoAppendFrame (frame, callback)

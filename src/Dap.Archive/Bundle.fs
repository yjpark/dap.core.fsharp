[<AutoOpen>]
[<RequireQualifiedAccess>]
module Dap.Archive.Bundle

open System.IO

open Dap.Prelude
open Dap.Context
open Dap.Platform
open Dap.Remote

type Spec<'extra, 'frame> when 'extra :> IJson and 'frame :> IFrame = {
    Kind : string
    Version : int
    ExtraDecoder : JsonDecoder<'extra>
    ReadFrame : ReadFrame<'frame>
}

type Param<'extra, 'frame> when 'extra :> IJson and 'frame :> IFrame = {
    Storage : IStorage<'extra>
    KeepVolumes : bool
    KeepVolumeFrames : bool
}

type Bundle<'extra, 'frame> when 'extra :> IJson and 'frame :> IFrame (spec', param') =
    let spec : Spec<'extra, 'frame> = spec'
    let param : Param<'extra, 'frame> = param'
    let mutable volume : Volume<'extra, 'frame> option = None
    let mutable volumes : Volume<'extra, 'frame> list = []
    let mutable volumeForWrite : Volume<'extra, 'frame> option = None
    member __.Spec with get () = spec
    member __.Param with get () = param
    member __.Volume with get () = volume
    member __.Volumes with get () = volumes

type Spec'<'extra, 'frame> when 'extra :> IJson and 'frame :> IFrame = {
    Kind : string
    Version : int
    CalcVolumeKey : Instant -> string
    VolumeDuration : Duration
    NewExtra : unit -> 'extra
    UpdateExtra : 'extra -> 'frame -> 'extra * 'frame
}

type Param'<'extra, 'frame> when 'extra :> IJson and 'frame :> IFrame = {
    Storage : IStorage'<'extra>
    KeepVolumes : bool
    KeepVolumeFrames : bool
}

type Bundle'<'extra, 'frame> when 'extra :> IJson and 'frame :> IFrame (spec', param') =
    let spec : Spec'<'extra, 'frame> = spec'
    let param : Param'<'extra, 'frame> = param'
    let mutable volume : Volume'<'extra, 'frame> option = None
    let mutable volumes : Volume'<'extra, 'frame> list = []
    let mutable isDirty : bool = false
    let closeVolume (runner : IRunner) : unit =
        match volume with
        | Some volume' ->
            volume'.Close runner
            runner.RunTask0 ignoreOnFailed <| param.Storage.WriteMetaAsync volume'.Meta
            volume <- None
        | None ->
            ()
    let checkVolume (runner : IRunner) (time : Instant) : unit =
        let key = spec.CalcVolumeKey time
        match volume with
        | Some volume' ->
            if volume'.Meta.Key <> key then
                closeVolume runner
        | None ->
            ()
        if volume.IsNone then
            let extra = spec.NewExtra ()
            let meta = newMeta spec.Kind key spec.Version extra runner.Clock.Now None
            let volumeParam : Volume.Param'<'frame> = {
                KeepFrames = param.KeepVolumeFrames
            }
            let volume' = new Volume'<'extra, 'frame> (volumeParam, meta)
            let stream = param.Storage.NewFramesStream runner key
            volume'.Open runner stream
            volume <- Some volume'
            if param.KeepVolumes then
                volumes <- volumes @ [ volume' ]
    member __.Spec with get () = spec
    member __.Param with get () = param
    member __.Volume with get () = volume
    member __.Volumes with get () = volumes
    member __.Open (runner : IRunner) (time : Instant) : unit =
        checkVolume runner time
    member __.Close (runner : IRunner) : unit =
        closeVolume runner
    member __.WriteFrame (runner : IRunner) (frame : 'frame) : unit =
        checkVolume runner frame.Time
        match volume with
        | Some volume ->
            isDirty <- true
            let (extra, frame) = spec.UpdateExtra volume.Meta.Extra frame
            volume.WriteFrame runner (extra, frame)
        | None ->
            logError runner "Bundle'.WriteFrame" "CheckVolume_Failed" frame
            failwith "Bundle'.WriteFrame Failed: CheckVolume_Failed"
    member __.Flush (runner : IRunner) : unit =
        if isDirty then
            isDirty <- false
            match volume with
            | Some volume' ->
                volume'.Flush runner
                runner.RunTask0 ignoreOnFailed <| param.Storage.WriteMetaAsync volume'.Meta
            | None ->
                logWarn runner "Bundle'.Flush" "No_Volume" ()

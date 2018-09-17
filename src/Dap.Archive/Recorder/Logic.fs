[<RequireQualifiedAccess>]
module Dap.Archive.Recorder.Logic

open System.IO

open Dap.Prelude
open Dap.Context
open Dap.Platform
open Dap.Remote
open Dap.Archive
open Dap.Archive.Recorder.Types
module TickerTypes = Dap.Platform.Ticker.Types

type ActorOperate<'extra, 'frame when 'extra :> IJson and 'frame :> IFrame> =
    ActorOperate<Agent<'extra, 'frame>, Args, Model<'extra, 'frame>, Msg<'extra, 'frame>, Req<'extra, 'frame>, Evt<'extra, 'frame>>

let private doBeginRecording req ((bundle, callback) : Bundle'<'extra, 'frame> * Callback<Meta<'extra>>)
                            : ActorOperate<'extra, 'frame> =
    fun runner (model, cmd) ->
        match model.Bundle with
        | None -> ()
        | Some bundle ->
            bundle.Close runner
            bundle.Volume |> Option.iter (fun v ->
                runner.Deliver <| RecorderEvt ^<| OnFinishRecording v.Meta
            )
        bundle.Open runner runner.Clock.Now
        match bundle.Volume with
        | Some volume ->
            reply runner callback <| ack req volume.Meta
            runner.Deliver <| RecorderEvt ^<| OnBeginRecording volume.Meta
            ({model with Bundle = Some bundle}, cmd)
        | None ->
            reply runner callback <| nak req "Bundle_Open_Failed" bundle
            ({model with Bundle = None}, cmd)

let private doFinishRecording req (callback : Callback<Meta<'extra>>)
                            : ActorOperate<'extra, 'frame> =
    fun runner (model, cmd) ->
        match model.Bundle with
        | Some bundle ->
            bundle.Close runner
            match bundle.Volume with
            | Some volume ->
                reply runner callback <| ack req volume.Meta
                runner.Deliver <| RecorderEvt ^<| OnFinishRecording volume.Meta
            | None ->
                reply runner callback <| nak req "Bundle_Has_No_Volume" bundle
            ({model with Bundle = None}, cmd)
        | None ->
            reply runner callback <| nak req "Not_Recording" None
            (model, cmd)

let private doAppendFrame req ((frame, callback) : 'frame * Callback<Meta<'extra> * 'frame>)
                            : ActorOperate<'extra, 'frame> =
    fun runner (model, cmd) ->
        match model.Bundle with
        | Some bundle ->
            try
                bundle.WriteFrame runner frame
                match bundle.Volume with
                | Some volume ->
                    reply runner callback <| ack req (volume.Meta, frame)
                    runner.Deliver <| RecorderEvt ^<| OnAppendFrame (volume.Meta, frame)
                | None ->
                    reply runner callback <| nak req "Bundle_Has_No_Volume" (frame, bundle)
                    failwith "Bundle_Has_No_Volume"
            with e ->
                reply runner callback <| nak req "AppendFrame_Failed" (frame, bundle, e)
                runner.Deliver <| RecorderEvt ^<| OnAppendFrameFailed (frame, e)
        | None ->
            reply runner callback <| nak req "Not_Recording" None
        (model, cmd)

let private handleReq req : ActorOperate<'extra, 'frame> =
    fun runner (model, cmd) ->
        match req with
        | DoBeginRecording (a, b) -> doBeginRecording req (a, b)
        | DoFinishRecording a -> doFinishRecording req a
        | DoAppendFrame (a, b) -> doAppendFrame req (a, b)
        <| runner <| (model, cmd)

let private handleEvt evt : ActorOperate<'extra, 'frame> =
    fun runner (model, cmd) ->
        match evt with
        | OnAppendFrameFailed (_frame, _e) ->
            model.Bundle |> Option.iter (fun bundle -> bundle.Close runner)
            updateModel (fun m -> {m with Bundle = None})
        | _ -> noOperation
        <| runner <| (model, cmd)

let private onTick ((time, delta) : Instant * Duration) : ActorOperate<'extra, 'frame> =
    fun runner (model, cmd) ->
        model.Bundle
        |> Option.bind (fun bundle ->
            if runner.Clock.Now > model.NextFlushTime then
                bundle.Flush runner
                let nextFlushTime = runner.Clock.Now + runner.Actor.Args.FlushInterval
                Some <| updateModel (fun m -> {m with NextFlushTime = nextFlushTime})
            else
                None
        )|> Option.defaultValue noOperation
        <| runner <| (model, cmd)

let private update : ActorUpdate<Agent<'extra, 'frame>, Args, Model<'extra, 'frame>, Msg<'extra, 'frame>, Req<'extra, 'frame>, Evt<'extra, 'frame>> =
    fun runner msg model ->
        match msg with
        | RecorderReq req -> handleReq req
        | RecorderEvt evt -> handleEvt evt
        | OnTick (a, b) -> onTick (a, b)
        <| runner <| (model, [])

let private init : ActorInit<Args, Model<'extra, 'frame>, Msg<'extra, 'frame>> =
    fun runner args ->
        let ticker = runner.Env |> Env.getService args.TickerIdent.Kind args.TickerIdent.Key
        let ticker = ticker :?> TickerTypes.Agent
        ticker.WatchOnTick runner "OnTick" (runner.Deliver << OnTick)
        ({
            Bundle = None
            NextFlushTime = runner.Clock.Now + args.FlushInterval
        }, noCmd)

let spec<'extra, 'frame when 'extra :> IJson and 'frame :> IFrame> (args : Args) =
    new ActorSpec<Agent<'extra, 'frame>, Args, Model<'extra, 'frame>, Msg<'extra, 'frame>, Req<'extra, 'frame>, Evt<'extra, 'frame>>
        (Agent<'extra, 'frame>.Spawn, args, RecorderReq, castEvt<'extra, 'frame>, init, update)
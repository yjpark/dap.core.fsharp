[<RequireQualifiedAccess>]
module Dap.Archive.Recorder.Logic

open System.IO
open Elmish
open Dap.Prelude
open Dap.Platform
open Dap.Remote
open Dap.Archive

type ActorOperate<'extra, 'frame when 'extra :> JsonRecord and 'frame :> IFrame> =
    ActorOperate<Args<'extra, 'frame>, Model<'extra, 'frame>, Msg<'extra, 'frame>, Req<'extra, 'frame>, Evt<'extra, 'frame>>

let private doBeginRecording msg ((bundle, callback) : Bundle'<'extra, 'frame> * Callback<Meta<'extra>>)
                            : ActorOperate<'extra, 'frame> =
    fun runner (model, cmd) ->
        match model.Bundle with
        | None -> ()
        | Some bundle ->
            bundle.Close runner
            bundle.Volume |> Option.iter (fun v -> runner.Actor.Args.FireEvent' <| OnFinishRecording v.Meta)
        bundle.Open runner runner.Clock.Now
        match bundle.Volume with
        | Some volume ->
            reply runner callback <| ack msg volume.Meta
            runner.Actor.Args.FireEvent' <| OnBeginRecording volume.Meta
            ({model with Bundle = Some bundle}, cmd)
        | None ->
            reply runner callback <| nak msg "Bundle_Open_Failed" bundle
            ({model with Bundle = None}, cmd)

let private doFinishRecording msg (callback : Callback<Meta<'extra>>)
                            : ActorOperate<'extra, 'frame> =
    fun runner (model, cmd) ->
        match model.Bundle with
        | Some bundle ->
            bundle.Close runner
            match bundle.Volume with
            | Some volume ->
                reply runner callback <| ack msg volume.Meta
                runner.Actor.Args.FireEvent' <| OnFinishRecording volume.Meta
            | None ->
                reply runner callback <| nak msg "Bundle_Has_No_Volume" bundle
            ({model with Bundle = None}, cmd)
        | None ->
            reply runner callback <| nak msg "Not_Recording" None
            (model, cmd)

let private doAppendFrame msg ((frame, callback) : 'frame * Callback<Meta<'extra> * 'frame>)
                            : ActorOperate<'extra, 'frame> =
    fun runner (model, cmd) ->
        match model.Bundle with
        | Some bundle ->
            try
                bundle.WriteFrame runner frame
                match bundle.Volume with
                | Some volume ->
                    reply runner callback <| ack msg (volume.Meta, frame)
                    runner.Actor.Args.FireEvent' <| OnAppendFrame (volume.Meta, frame)
                | None ->
                    reply runner callback <| nak msg "Bundle_Has_No_Volume" (frame, bundle)
                    failwith "Bundle_Has_No_Volume"
            with e ->
                reply runner callback <| nak msg "AppendFrame_Failed" (frame, bundle, e)
                runner.Actor.Args.FireEvent' <| OnAppendFrameFailed (frame, e)
        | None ->
            reply runner callback <| nak msg "Not_Recording" None
        (model, cmd)

let private handleReq msg req : ActorOperate<'extra, 'frame> =
    fun runner (model, cmd) ->
        match req with
        | DoBeginRecording (a, b) -> doBeginRecording msg (a, b)
        | DoFinishRecording a -> doFinishRecording msg a
        | DoAppendFrame (a, b) -> doAppendFrame msg (a, b)
        <| runner <| (model, cmd)

let private handleEvt _msg evt : ActorOperate<'extra, 'frame> =
    fun runner (model, cmd) ->
        match evt with
        | OnAppendFrameFailed (_frame, _e) -> 
            model.Bundle |> Option.iter (fun bundle -> bundle.Close runner)
            setModel {model with Bundle = None}
        | _ -> noOperation
        <| runner <| (model, cmd)

let private update : ActorUpdate<Args<'extra, 'frame>, Model<'extra, 'frame>, Msg<'extra, 'frame>, Req<'extra, 'frame>, Evt<'extra, 'frame>> =
    fun runner model msg -> 
        match msg with
        | RecorderReq req -> handleReq msg req
        | RecorderEvt evt -> handleEvt msg evt
        <| runner <| (model, [])

let private init : ActorInit<Args<'extra, 'frame>, Model<'extra, 'frame>, Msg<'extra, 'frame>, Req<'extra, 'frame>, Evt<'extra, 'frame>> =
    fun _runner args ->
        ({
            Bundle = None
        }, noCmd)

let private subscribe : ActorSubscribe<Args<'extra, 'frame>, Model<'extra, 'frame>, Msg<'extra, 'frame>, Req<'extra, 'frame>, Evt<'extra, 'frame>> =
    fun runner model ->
        subscribeEvent runner model RecorderEvt runner.Actor.Args.OnEvent

let logic =
    {
        Init = init
        Update = update
        Subscribe = subscribe
    }

let getSpec (newArgs : NewArgs<Args<'extra, 'frame>>) : AgentSpec<Args<'extra, 'frame>, Model<'extra, 'frame>, Msg<'extra, 'frame>, Req<'extra, 'frame>, Evt<'extra, 'frame>> =
    {
        Actor =
            {
                NewArgs = newArgs
                Logic = logic
                WrapReq = RecorderReq
                GetOnEvent = fun args -> args.OnEvent
            }
        OnAgentEvent = None
        GetSlowCap = None
    }
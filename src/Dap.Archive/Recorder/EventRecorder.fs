[<RequireQualifiedAccess>]
module Dap.Archive.Recorder.EventRecorder

open Dap.Prelude
open Dap.Context
open Dap.Platform
open Dap.Remote
open Dap.Archive

open Dap.Archive.Recorder.Types

[<Literal>]
let Kind = "EventRecorder"
[<Literal>]
let Version = 1

type Extra = {
    Events : Map<string, int>
} with
    static member Create events = {
        Events = events
    }
    static member JsonDecoder (extraDecoder : JsonDecoder<'extra>) =
        D.decode Extra.Create
        |> D.required "events" (D.dict D.int)
    interface IJson with
        member this.ToJson () =
            let events = this.Events |> Map.map (fun k v -> E.int v)
            E.object [
                "events", E.dict events
            ]

type Meta = Meta<Extra>
type Frame = PacketFrame
type Model = Model<Extra, Frame>
type Req = Req<Extra, Frame>
type Evt = Evt<Extra, Frame>
type Msg = Msg<Extra, Frame>

type IStorage' = IStorage'<Extra>

type Volume' = Volume'<Extra, Frame>

type BundleSpec' = Bundle.Spec'<Extra, Frame>
type BundleParam' = Bundle.Param'<Extra, Frame>
type Bundle' = Bundle'<Extra, Frame>

type Args = Dap.Archive.Recorder.Types.Args
type Agent = IAgent<Req<Extra, Frame>, Evt<Extra, Frame>>

let newExtra () =
    {
        Events = Map.empty
    }

let updateExtra (extra : Extra) (frame : Frame) : Extra * Frame =
    let count =
        extra.Events
        |> Map.tryFind frame.Kind
        |> Option.defaultValue 0
    let count = count + 1
    let events =
        extra.Events
        |> Map.add frame.Kind count
    ({extra with Events = events}, {frame with Id = count.ToString ()})

let registerAsync' kind args env =
    let spec = Logic.spec<Extra, Frame> args
    env |> Env.registerAsync spec kind

let registerAsync a = registerAsync' Kind a

let appendEvent' (agent : Agent) (kind : string) (payload : string) : unit =
    let frame = PacketFrame.Create agent.Env.Clock.Now "" kind payload
    agent.Post <| DoAppendFrame frame None

let appendEvent (agent : Agent) (evt : IEvent) : unit =
    appendEvent' agent evt.Kind <| evt.EncodeJson 4

let watchEvents (agent : Agent)
                (onEvent : IEvent<'evt> when 'evt :> IEvent)
                (kinds : string list) : unit =
    let kinds = Set.ofList kinds
    onEvent.Add (fun evt ->
        let evt = evt :> IEvent
        if kinds |> Set.contains evt.Kind then
            appendEvent agent evt
    )

let getBundleSpec' (profile : Profile) : BundleSpec' =
    {
        Kind = Kind
        Version = 1
        CalcVolumeKey = profile.CalcVolumeKey
        VolumeDuration = profile.VolumeDuration
        NewExtra = newExtra
        UpdateExtra = updateExtra
    }

let createBundle' (profile : Profile) (param : BundleParam') : Bundle' =
    let spec = getBundleSpec' profile
    new Bundle' (spec, param)
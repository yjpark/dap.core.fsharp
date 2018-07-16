module Dap.Archive.WebSocket.Accessor.Types

open System.Threading
open Dap.Platform
open Dap.Archive.Recorder

module WebSocketTypes = Dap.WebSocket.Types
module WebSocketClientTypes = Dap.WebSocket.Client.Types
module EventRecorder = Dap.Archive.Recorder.EventRecorder

type Client<'pkt> = Dap.WebSocket.Client.Types.Agent<'pkt>

type Args<'pkt> = {
    ClientKind : Kind
    RetryDelay : float<second> option
    CreateRecorderAsync : GetTask<Client<'pkt>, EventRecorder.Agent option> option
    OnConnectedAsync : GetTask<Client<'pkt>, unit> option
} with
    static member Create clientKind =
        {
            ClientKind = clientKind
            RetryDelay = None
            CreateRecorderAsync = None
            OnConnectedAsync = None
        }
    member this.WithRetryDelay v = {this with RetryDelay = Some v}
    member this.WithCreateRecorderAsync v = {this with CreateRecorderAsync = Some v}
    member this.WithOnConnectedAsync v = {this with OnConnectedAsync = Some v}

and Model<'pkt> = {
    Uri : string option
    Cts : CancellationTokenSource
    Client : Client<'pkt> option
    Recorder : EventRecorder.Agent option
    Running : bool
    Reconnecting : bool
} with
    member this.Connected =
        this.Client
        |> Option.map (fun c -> c.Actor.State.Connected)
        |> Option.defaultValue false

and Req<'pkt> =
    | DoSetup of string * Callback<unit>
    | DoStart of Callback<WebSocketTypes.ConnectedStats option>
    | DoStop of Callback<unit>
    | DoSend of 'pkt * Callback<WebSocketTypes.SendStats>
with interface IReq

and Evt<'pkt> =
    | OnStarted of WebSocketTypes.ConnectedStats
    | OnStopped of WebSocketTypes.ConnectionStats option
    | OnSent of WebSocketTypes.SendStats * 'pkt
    | OnReceived of WebSocketTypes.ReceiveStats * 'pkt
with interface IEvt

and InternalEvt<'pkt> =
    | OnSetup of  IReq * Callback<unit> * Client<'pkt> * (EventRecorder.Agent option)
    | OnConnected of WebSocketTypes.ConnectedStats
    | OnDisconnected of WebSocketTypes.ConnectionStats option
    | DoReconnect

and Msg<'pkt> =
    | AccessorReq of Req<'pkt>
    | AccessorEvt of Evt<'pkt>
    | InternalEvt of InternalEvt<'pkt>
with interface IMsg

let castEvt<'pkt> : CastEvt<Msg<'pkt>, Evt<'pkt>> =
    function
    | AccessorEvt evt -> Some evt
    | _ -> None

let DoSetup' uri callback =
    DoSetup (uri, callback)

let DoSend' pkt callback =
    DoSend (pkt, callback)

type Part<'actorMsg, 'pkt when 'actorMsg :> IMsg> (param) =
    inherit BasePart<'actorMsg, Part<'actorMsg, 'pkt>, Args<'pkt>, Model<'pkt>, Msg<'pkt>, Req<'pkt>, Evt<'pkt>> (param)
    override this.Runner = this
    static member Spawn (param) = new Part<'actorMsg, 'pkt> (param)

type PartOperate<'actorMsg, 'pkt when 'actorMsg :> IMsg> =
    ActorOperate<Part<'actorMsg, 'pkt>, Args<'pkt>, Model<'pkt>, Msg<'pkt>, Req<'pkt>, Evt<'pkt>>

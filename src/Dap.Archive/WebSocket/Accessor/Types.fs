module Dap.Archive.WebSocket.Accessor.Types

open System.Threading
open Dap.Platform
open Dap.Archive.Recorder

module WebSocketTypes = Dap.WebSocket.Types
module WebSocketClientTypes = Dap.WebSocket.Client.Types
module EventRecorder = Dap.Archive.Recorder.EventRecorder

type TextClient<'pkt> = Dap.WebSocket.Client.Types.Agent<'pkt>

type Runner<'pkt> = IModRunner<Args, Model<'pkt>, Msg<'pkt>>

and ModOperate<'pkt> = ModOperate<Args, Model<'pkt>, Msg<'pkt>>

and Args = {
    ClientKind : Kind
    RetryDelay : float<second> option
    CreateRecorderAsync : GetTask<IAgent, EventRecorder.Agent> option
} with
    static member Create clientKind retryDelay createRecorderAsync =
        {
            ClientKind = clientKind
            RetryDelay = retryDelay
            CreateRecorderAsync = createRecorderAsync
        }

and Model<'pkt> = {
    Uri : string option
    Cts : CancellationTokenSource
    Client : TextClient<'pkt> option
    Recorder : EventRecorder.Agent option
    Running : bool
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
    | OnSetup of  IReq * Callback<unit> * TextClient<'pkt> * (EventRecorder.Agent option)
    | OnConnected of WebSocketTypes.ConnectedStats
    | OnDisconnected of WebSocketTypes.ConnectionStats option
    | DoReconnect

and Msg<'pkt> =
    | AccessorReq of Req<'pkt>
    | AccessorEvt of Evt<'pkt>
    | InternalEvt of InternalEvt<'pkt>
with interface IMsg
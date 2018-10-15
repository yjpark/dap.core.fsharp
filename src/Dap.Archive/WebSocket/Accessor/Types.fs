module Dap.Archive.WebSocket.Accessor.Types

open System.Threading

open Dap.Context
open Dap.Platform
open Dap.WebSocket
open Dap.Archive.Recorder

module WebSocketTypes = Dap.WebSocket.Types
module WebSocketClientTypes = Dap.WebSocket.Client.Types
module EventRecorder = Dap.Archive.Recorder.EventRecorder

type Client<'pkt> = Dap.WebSocket.Client.Types.Agent<'pkt>

type Args<'pkt> = {
    ClientKind : Kind
    RetryDelay : float<second> option
    CreateRecorderAsync : GetTask<Client<'pkt>, EventRecorder.Agent option> option
    OnLinkedAsync : GetTask<Client<'pkt>, unit> option
} with
    static member Create clientKind =
        {
            ClientKind = clientKind
            RetryDelay = None
            CreateRecorderAsync = None
            OnLinkedAsync = None
        }
    member this.WithRetryDelay v = {this with RetryDelay = Some v}
    member this.WithCreateRecorderAsync v = {this with CreateRecorderAsync = Some v}
    member this.WithOnLinkedAsync v = {this with OnLinkedAsync = Some v}

and Model<'pkt> = {
    Uri : string option
    Cts : CancellationTokenSource
    Client : Client<'pkt> option
    Recorder : EventRecorder.Agent option
    Running : bool
    Reconnecting : bool
} with
    member this.Status =
        this.Client
        |> Option.map (fun c -> c.Actor.State.Status)
        |> Option.defaultValue LinkStatus.NoLink

and Req<'pkt> =
    | DoSetup of string * Callback<unit>
    | DoSetUri of string * Callback<unit>
    | DoStart of Callback<unit>
    | DoStop of Callback<unit>
    | DoSend of 'pkt * Callback<unit>
with interface IReq

and Evt<'pkt> =
    | OnStarted
    | OnStopped
    | OnSent of 'pkt
    | OnReceived of 'pkt
with interface IEvt

and InternalEvt<'pkt> =
    | OnSetup of  IReq * Callback<unit> * Client<'pkt> * (EventRecorder.Agent option)
    | OnLinked
    | OnClosed
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

let DoSetup uri callback =
    DoSetup (uri, callback)

let DoSetUri uri callback =
    DoSetUri (uri, callback)

let DoSend pkt callback =
    DoSend (pkt, callback)

type Part<'actorMsg, 'pkt when 'actorMsg :> IMsg> (param) =
    inherit BasePart<'actorMsg, Part<'actorMsg, 'pkt>, Args<'pkt>, Model<'pkt>, Msg<'pkt>, Req<'pkt>, Evt<'pkt>> (param)
    let mutable clientStats : LinkStats option = None
    override this.Runner = this
    static member Spawn (param) = new Part<'actorMsg, 'pkt> (param)
    member this.SetClientStats' (client : Client<'pkt>) =
        let clientStats' = this.Agent.Console.Stats.Target.AddLink (client.LinkStats, "client")
        clientStats <- Some client.LinkStats
    member __.ClientStats : LinkStats option = clientStats

type PartOperate<'actorMsg, 'pkt when 'actorMsg :> IMsg> =
    Operate<Part<'actorMsg, 'pkt>, Model<'pkt>, Msg<'pkt>>

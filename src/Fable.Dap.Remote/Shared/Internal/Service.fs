[<AutoOpen>]
[<RequireQualifiedAccess>]
module Dap.Remote.Internal.Service

open Dap.Prelude
open Dap.Platform
open Dap.Remote
open Dap.Remote.Internal

type Hub = {
    OnRequest : Packet -> unit
}

type Link = {
    Send : Packet -> LocalReason option
}

type Args = {
    Link : Link
    Hub : Hub
    Logger : ILogger
    LogTraffic : bool
}

type Model = {
    Args : Args
}

type Msg =
    | DoSendResponse of PacketId * Result<IResponse, HubReason>
    | DoSendEvent of IEvent
    | OnSent of Packet
    | OnReceived of Packet

let create args =
    {
        Args = args
    }

let private doSendPacket' (section : string) (pkt: Packet) (model : Model) : Model =
    try
        match model.Args.Link.Send pkt with
        | Some err ->
            logError model.Args.Logger section "Link_Failed" (pkt, err)
        | None ->
            ()
    with e ->
        logException model.Args.Logger section "Exception_Raised" pkt e
    model

let private doSendPacket (section : string) (pkt: Packet option) =
    match pkt with
    | None -> id
    | Some pkt ->
        doSendPacket' section pkt

let private doSendEvent (evt : IEvent) (model : Model) : Model =
    let mutable pkt = None
    try
        pkt <- Some {
            Time = dateTimeUtcNow ()
            Id = Const.IdEvt
            Kind = evt.Kind
            Payload = evt.Payload
        }
    with e ->
        logException model.Args.Logger "Send_Event" "Encode_Failed" evt e
    doSendPacket "Send_Event" pkt model

let private doSendResponse (requestId : PacketId) (res : Result<IResponse, HubReason>) (model : Model) : Model =
    let mutable pkt = None
    try
        let (kind, payload) =
            match res with
            | Ok res ->
                (Const.KindAck, res.Payload)
            | Error reason ->
                match reason with
                | HubNak nak ->
                    (Const.KindNak, nak.Payload)
                | HubError err ->
                    (Const.KindErr, err.Payload)
                | HubException exn ->
                    (Const.KindExn, exn.Payload)
        pkt <- Some {
            Time = dateTimeUtcNow ()
            Id = requestId
            Kind = kind
            Payload = payload
        }
    with e ->
        logException model.Args.Logger "Send_Response" "Encode_Failed" (requestId, res) e
    doSendPacket "Send_Response" pkt model

let private onReceived (pkt : Packet) (model : Model) : Model =
    if model.Args.LogTraffic then
        logInfo model.Args.Logger "Traffic" "Received" pkt
    try
        model.Args.Hub.OnRequest pkt
        model
    with e ->
        logException model.Args.Logger "Receive" "Exception_Raised" pkt e
        doSendResponse pkt.Id (Error <| HubException ^<| ExnJson.OfExn e) model

let private onSent (pkt : Packet) (model : Model) : Model =
    if model.Args.LogTraffic then
        logInfo model.Args.Logger "Traffic" "Sent" pkt
    model

let handle (msg : Msg) : Model -> Model =
    match msg with
    | DoSendResponse (requestId, res) ->
        doSendResponse requestId res
    | DoSendEvent evt ->
        doSendEvent evt
    | OnSent pkt ->
        onSent pkt
    | OnReceived pkt ->
        onReceived pkt
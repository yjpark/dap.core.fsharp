[<AutoOpen>]
[<RequireQualifiedAccess>]
module Dap.Remote.Internal.Gateway

open System
open Dap.Prelude
open Dap.Context
open Dap.Platform
open Dap.Remote
open Dap.Remote.Internal

type Hub = {
    OnRequest : PacketId * Json -> unit
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
    | DoSendResponse of PacketId * Result<IResult, HubReason>
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
            Id = newGuid ()
            Kind = Const.KindEvt
            Payload = toJson evt
        }
    with e ->
        logException model.Args.Logger "Send_Event" "Encode_Failed" evt e
    doSendPacket "Send_Event" pkt model

let private doSendResponse (requestId : PacketId) (res : Result<IResult, HubReason>) (model : Model) : Model =
    let mutable pkt = None
    try
        let (kind, payload) =
            match res with
            | Ok res ->
                (Const.KindRes, toJson res)
            | Error reason ->
                match reason with
                | HubNak nak ->
                    (Const.KindNak, toJson nak)
                | HubError err ->
                    (Const.KindErr, toJson err)
                | HubException exn ->
                    (Const.KindExn, toJson exn)
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
    if pkt.Kind = Const.KindReq then
        try
            model.Args.Hub.OnRequest (pkt.Id, pkt.Payload)
            model
        with e ->
            logException model.Args.Logger "Receive" "Exception_Raised" pkt e
            doSendResponse pkt.Id (Error <| HubException ^<| ExnJson.OfExn e) model
    else
        logError model.Args.Logger "Receive" "Invalid_Kind" (pkt.Kind, pkt)
        model

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
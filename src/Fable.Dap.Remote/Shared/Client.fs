[<RequireQualifiedAccess>]
module Dap.Remote.Client

open System
open Dap.Prelude

type PendingRequest = {
    Req : IRequest
    Packet : Packet'
    SentTime : DateTime
}

type Stub = {
    OnResponse : IRequest * Result<string, Reason'> -> unit
    OnEvent : PacketKind * string -> unit
}

type Link = {
    Send : IRequest * Packet' -> LocalReason option
}

type Args = {
    Link : Link
    Stub : Stub
    Logger : ILogger
    LogTraffic : bool
}

type Model = {
    Args : Args
    PendingRequests : Map<PacketId, PendingRequest>
}

type Msg =
    | DoSend of IRequest
    | OnSent of IRequest * Packet' * Result<DateTime, LocalReason>
    | OnReceived of Packet'

let create args =
    {
        Args = args
        PendingRequests = Map.empty
    }

let private addPendingRequest (req : PendingRequest) (model : Model) : Model =
    let requests = model.PendingRequests |> Map.add req.Packet.Id req
    {model with PendingRequests = requests}

let private removePendingRequest (packetId : PacketId) (model : Model) : Model =
    let requests = model.PendingRequests |> Map.remove packetId
    {model with PendingRequests = requests}

let private doSend (req : IRequest) (model : Model) : Model =
    let mutable encoded : Packet' option = None
    try
        let pkt = {
            Id = Guid.NewGuid().ToString()
            Kind = req.Kind
            Payload = req.Payload
        }
        encoded <- Some pkt
        match model.Args.Link.Send (req, pkt) with
        | Some err ->
            logError model.Args.Logger "Send" "Link_Failed" (req, err)
            model.Args.Stub.OnResponse (req, Error <| Local' err)
        | None ->
            ()
    with e ->
        match encoded with
        | None ->
            logException model.Args.Logger "Send" "Encode_Failed" req e
            model.Args.Stub.OnResponse (req, Error <| Local' ^<| EncodeFailed e.Message)
        | Some packet ->
            logException model.Args.Logger "Send" "Exception_Raised" packet e
            model.Args.Stub.OnResponse (req, Error <| Local' ^<| LocalException e.Message)
    model

let private onSend ((req, pkt, res) : IRequest * Packet' * Result<DateTime, LocalReason>) (model : Model) : Model =
    match res with
    | Ok time ->
        if model.Args.LogTraffic then
            logInfo model.Args.Logger "Traffic" "Sent" pkt
        model |> addPendingRequest {
            Req = req
            Packet = pkt
            SentTime = time
        }
    | Error reason ->
        logError model.Args.Logger "Send" "Link_Failed" (req, pkt)
        model.Args.Stub.OnResponse (req, Error <| Local' reason)
        model

let private onReceived (pkt : Packet') (model : Model) : Model =
    if model.Args.LogTraffic then
        logInfo model.Args.Logger "Traffic" "Received" pkt
    if pkt.Id = Const.IdEvt then
        try
            model.Args.Stub.OnEvent (pkt.Kind, pkt.Payload)
        with e ->
            logException model.Args.Logger "Receive" "Stub_OnEvent_Failed" pkt e
        model
    else
        let mutable found = None
        try
            let req = (model.PendingRequests |> Map.find pkt.Id)
            found <- Some req
            let res =
                match pkt.Kind with
                | Const.KindAck ->
                    Ok <| pkt.Payload
                | Const.KindBad ->
                    Error <| Remote' ^<| BadRequest' pkt.Payload
                | Const.KindNak ->
                    Error <| Remote' ^<| RemoteFailed' pkt.Payload
                | Const.KindExn ->
                    Error <| Remote' ^<| RemoteException' pkt.Payload
                | _ ->
                    Error <| Remote' ^<| InvalidKind' pkt.Kind
            model.Args.Stub.OnResponse (req.Req, res)
        with e ->
            match found with
            | None ->
                logException model.Args.Logger "Receive" "Request_Not_Found" pkt e
            | Some req ->
                logException model.Args.Logger "Receive" "Stub_OnResponse_Failed" (req, pkt) e
                model.Args.Stub.OnResponse (req.Req, Error <| Local' ^<| LocalException e.Message)
        match found with
        | None ->
            model
        | Some req ->
            model |> removePendingRequest req.Packet.Id

let handle (msg : Msg) : Model -> Model =
    match msg with
    | DoSend req ->
        doSend req
    | OnSent (req, pkt, res) ->
        onSend (req, pkt, res)
    | OnReceived pkt ->
        onReceived pkt
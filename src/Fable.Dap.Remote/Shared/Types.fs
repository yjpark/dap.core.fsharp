namespace Dap.Remote

type Second = float

open Dap.Prelude
open Dap.Platform

module Const =
    [<Literal>]
    let KindUnknown = "N/A"

    [<Literal>]
    let KindAck = "Ack"

    [<Literal>]
    let KindBad = "Bad"

    [<Literal>]
    let KindNak = "Nak"

    [<Literal>]
    let KindExn = "Exn"

    [<Literal>]
    let IdEvt = "Evt"

type PacketId = string
type PacketKind = string

type IRequest =
    abstract Kind : PacketKind with get
    abstract Payload : string with get

type IResponse =
    abstract Payload : string with get

type IError =
    abstract Payload : string with get

type IEvent =
    abstract Kind : PacketKind with get
    abstract Payload : string with get

type InvalidRequest = InvalidRequest
    with
        interface IRequest with
            member _this.Kind = Const.KindUnknown
            member _this.Payload = ""

type LocalReason =
    | EncodeFailed of string
    | DecodeFailed of string
    | SendFailed of string
    | LocalException of string

type RemoteReason' =
    | InvalidKind' of string
    | BadRequest' of string
    | RemoteFailed' of string
    | RemoteException' of string
    | Timeout' of Second

type Reason' =
    | Local' of LocalReason
    | Remote' of RemoteReason'

(*
In Fable, have a type with same name as module
is not working, it will compile, though will get
runtime error, seems it access the type instead
of the module.
 *)
[<StructuredFormatDisplay("<Agent>{AsDisplay}")>]
type Packet' = {
    Id : PacketId
    Kind : PacketKind
    Payload : string
} with
    override this.ToString () =
        sprintf "[Packet: <%s> <%s> %s]" this.Id this.Kind <| String.capped 256 this.Payload
    member this.AsDisplay = this.ToString ()

type NoRemoteError = NoRemoteError
    with
        interface IError with
            member _this.Payload = ""

type RemoteReason<'err> =
    | InvalidKind of string
    | BadRequest of string
    | RemoteFailed of 'err
    | RemoteException of string
    | Timeout of Second

type Reason<'err> =
    | Local of LocalReason
    | Remote of RemoteReason<'err>

type StubResult<'res, 'err> = Result<'res, Reason<'err>>

type LocalStubErr<'res> = IRequest -> LocalReason -> 'res
type RemoteStubErr<'res> = IRequest -> RemoteReason' -> 'res
type DecodeStubRes<'res> = IRequest -> string -> 'res
type DecodeStubEvt<'evt> = PacketKind -> string -> 'evt

type StubSpec<'res, 'evt> = {
    LocalErr : LocalStubErr<'res>
    RemoteErr : RemoteStubErr<'res>
    DecodeRes : DecodeStubRes<'res>
    DecodeEvt : DecodeStubEvt<'evt>
}

type HubReason =
    | HubBad of string
    | HubFailed of IError
    | HubException of string

type Hub<'req, 'evt> = {
    PostReq : 'req -> unit
    OnEvent : IBus<'evt>
    OnDisconnected : unit -> unit
}

type OnHandled = Result<IResponse, HubReason> -> unit
type DecodeHubReq<'req> = PacketKind -> string -> OnHandled -> 'req
type GetHub<'req, 'evt> = string -> (Hub<'req, 'evt> -> unit) -> unit

type HubSpec<'req, 'evt> = {
    DecodeReq : DecodeHubReq<'req>
    GetHub : GetHub<'req, 'evt>
}

type BoolResponse =
    BoolResponse of bool
with
    static member Decode (str : string) =
        if str = "true" then
            BoolResponse true
        else
            BoolResponse false
    member this.Payload =
        let (BoolResponse v) = this
        if v then "true" else "false"
    interface IResponse with
        member this.Payload = this.Payload

type IntResponse =
    IntResponse of int
with
    static member Decode (str : string) =
        IntResponse <| int str
    member this.Payload =
        let (IntResponse v) = this
        string v
    interface IResponse with
        member this.Payload = this.Payload

type StringResponse =
    StringResponse of string
with
    static member Decode (str : string) =
        StringResponse str
    member this.Payload =
        let (StringResponse v) = this
        v
    interface IResponse with
        member this.Payload = this.Payload


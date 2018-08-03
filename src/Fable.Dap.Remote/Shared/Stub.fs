[<AutoOpen>]
[<RequireQualifiedAccess>]
module Dap.Remote.Stub

open Microsoft.FSharp.Reflection
#if FABLE_COMPILER
open Fable.Core
module E = Thoth.Json.Encode
module D = Thoth.Json.Decode
#else
module E = Thoth.Json.Net.Encode
module D = Thoth.Json.Net.Decode
#endif

open Dap.Prelude
open Dap.Platform
open Dap.Remote
open Dap.Remote.Internal

type LocalReason =
    | EncodeFailed of string
    | DecodeFailed of string
    | SendFailed of string
    | LocalException of msg : string * trace : string

type RemoteReason<'err> =
    | InvalidKind of string
    | RemoteNak of err : string * detail : string
    | RemoteError of 'err
    | RemoteException of msg : string * trace : string
    | Timeout of float<second>

type Reason<'err> =
    | Local of LocalReason
    | Remote of RemoteReason<'err>

type RemoteReason' =
    | InvalidKind' of string
    | RemoteNak' of Json
    | RemoteError' of Json
    | RemoteException' of Json
    | Timeout' of float<second>

type StubResult<'res, 'err> = Result<'res, Reason<'err>>

type LocalStubErr<'res> = IRequest -> LocalReason -> 'res
type RemoteStubErr<'res> = IRequest -> RemoteReason' -> 'res
type DecodeStubRes<'res> = IRequest -> Json -> 'res
type DecodeStubEvt<'evt> = PacketKind -> Json -> 'evt

type StubSpec<'res, 'evt> when 'evt :> IEvent = {
    LocalErr : LocalStubErr<'res>
    RemoteErr : RemoteStubErr<'res>
    DecodeRes : DecodeStubRes<'res>
    DecodeEvt : DecodeStubEvt<'evt>
}

type IProxy<'req, 'res, 'evt> when 'req :> IRequest and 'evt :> IEvent =
    inherit IAgent<'req, 'evt>
    abstract Connected : bool with get
    abstract OnResponse : IBus<'res>

type ResponseSpec<'res> = {
    Kind : string
    Case : UnionCaseInfo
    DecodeParam : Json -> obj
    GetResResult : Json -> obj
    GetErrResult : RemoteReason' -> obj
} with
#if FABLE_COMPILER
    [<PassGenericsAttribute>]
#endif
    static member Create<'param, 'result, 'error>
                            (kind : PacketKind) (case : PacketKind)
                            (paramDecoder : JsonDecoder<'param>)
                            (resDecoder : JsonDecoder<'result>)
                            (errDecoder : JsonDecoder<'error>) : ResponseSpec<'res> =
        let case =
            case
            |> Union.tryFindCase<'res>
            |> Result.get
        let decodeParam = fun (json : Json) ->
            castJson paramDecoder json
            :> obj
        let getResResult = fun (json : Json) ->
            castJson resDecoder json
            |> Ok
            :> obj
        let getErrResult = fun (reason : RemoteReason') ->
            match reason with
            | InvalidKind' kind ->
                InvalidKind kind
            | RemoteNak' json ->
                castJson NakJson.JsonDecoder json
                |> fun nak -> (nak.Err, nak.Detail)
                |> RemoteNak
            | RemoteError' json ->
                castJson errDecoder json
                |> RemoteError
            | RemoteException' json ->
                castJson ExnJson.JsonDecoder json
                |> fun exn -> (exn.Msg, exn.Trace)
                |> RemoteException
            | Timeout' seconds ->
                Timeout seconds
            |> Remote
            |> Error
            :> obj
        {
            Kind = kind
            Case = case
            DecodeParam = decodeParam
            GetResResult = getResResult
            GetErrResult = getErrResult
        }

type EventSpec<'evt> = {
    Case : UnionCaseInfo
    DecodeEvt : Json -> obj
} with
#if FABLE_COMPILER
    [<PassGenericsAttribute>]
#endif
    static member Create<'event>
                            (kind : PacketKind)
                            (evtDecoder : JsonDecoder<'event>) : EventSpec<'evt> =
        let case =
            kind
            |> Union.tryFindCase<'evt>
            |> Result.get
        let decodeEvt = fun (json : Json) ->
            castJson evtDecoder json
            :> obj
        {
            Case = case
            DecodeEvt = decodeEvt
        }

let private spawnRes (spec : ResponseSpec<'res> list)
                        (req : IRequest)
                        (getResult : ResponseSpec<'res> -> obj) =
    spec
    |> List.find (fun s -> s.Kind = req.Kind)
    |> (fun spec ->
        let param = spec.DecodeParam req.Payload
        let result = getResult spec
        FSharpValue.MakeUnion(spec.Case, [| param ; result |]) :?> 'res
    )

let localErr (spec : ResponseSpec<'res> list) : LocalStubErr<'res> =
    fun req reason ->
        spawnRes spec req (fun _s ->
            Local reason
            |> Error
            :> obj
        )

let remoteErr (spec : ResponseSpec<'res> list) : RemoteStubErr<'res> =
    fun req reason ->
        spawnRes spec req (fun s -> s.GetErrResult reason)

let decodeRes (spec : ResponseSpec<'res> list) : DecodeStubRes<'res> =
    fun req payload ->
        spawnRes spec req (fun s -> s.GetResResult payload)

let decodeEvt (spec : EventSpec<'evt> list) : DecodeStubEvt<'evt> =
    fun kind payload ->
        spec
        |> List.find (fun s -> s.Case.Name = kind)
        |> (fun spec ->
            let event = spec.DecodeEvt payload
            FSharpValue.MakeUnion(spec.Case, [| event |]) :?> 'evt
        )

let getReasonContent (reason : Reason<'err> when 'err :> IError) : string * string * string option =
    match reason with
    | Local reason ->
        match reason with
        | EncodeFailed message -> ("Encode Failed", message, None)
        | DecodeFailed message -> ("Decode Failed", message, None)
        | SendFailed message -> ("Send Failed", message, None)
        | LocalException (msg, trace) -> ("Local Exception", msg, Some trace)
    | Remote reason ->
        match reason with
        | InvalidKind kind -> ("Invalid Kind", kind, None)
        | RemoteNak (err, detail) -> ("Remote Nak", err, Some detail)
        | RemoteError err -> ("Remote Error", E.encode 4 err.Payload, None)
        | RemoteException (msg, trace) -> ("Remote Exception", msg, Some trace)
        | Timeout seconds -> ("Timeout", sprintf "%A" seconds, None)

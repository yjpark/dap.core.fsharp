[<AutoOpen>]
[<RequireQualifiedAccess>]
module Dap.Remote.Stub

open Microsoft.FSharp.Reflection
#if FABLE_COMPILER
open Fable.Core
#endif

open Dap.Prelude
open Dap.Context
open Dap.Platform
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
with
    member this.AsError : 'err option =
        match this with
        | Local _ -> None
        | Remote reason ->
            match reason with
            | RemoteError err -> Some err
            | _ -> None

type RemoteReason' =
    | InvalidKind' of string
    | RemoteNak' of Json
    | RemoteError' of Json
    | RemoteException' of Json
    | Timeout' of float<second>

type Reason' =
    | Local' of LocalReason
    | Remote' of RemoteReason'

type StubResult<'res, 'err> = Result<'res, Reason<'err>>

type IProxy<'req, 'res, 'evt> when 'req :> IRequest and 'evt :> IEvent =
    inherit IAgent<'req, 'evt>
    abstract Status : LinkStatus with get
    abstract OnStatus : IBus<LinkStatus>
    abstract OnResponse : IBus<'res>

type ResponseSpec<'res> = {
    Kind : string
    Case : UnionCaseInfo
    ParamDecoder : JsonDecoder<obj array>
    GetResResult : Json -> obj
    GetErrResult : RemoteReason' -> obj
    GetErrReason : LocalReason -> obj
} with
    static member Create<'param, 'result, 'error>
            (
                kind : PacketKind,
                fields : FieldSpec list,
                case : PacketKind,
                resDecoder : JsonDecoder<'result>,
                errDecoder : JsonDecoder<'error>
            #if FABLE_COMPILER
                , [<Inject>] ?resolver: ITypeResolver<'res>
            #endif
            ) : ResponseSpec<'res> =
    #if FABLE_COMPILER
        let resultType = "'result"
        let errorType = "'error"
    #else
        let resultType = typeof<'result>.FullName
        let errorType = typeof<'error>.FullName
    #endif
    #if FABLE_COMPILER
        let case = Union.findCase<'res> (case, ?resolver=resolver)
    #else
        let case = Union.findCase<'res> case
    #endif
        let getResResult = fun (json : Json) ->
            let res : StubResult<'result, 'error> =
                tryCastJson resDecoder json
                |> Result.mapError (fun err ->
                    sprintf "Stub.DecodeRes<%s> -> %s" resultType err
                )|> Result.get
                |> Ok
            res :> obj
        let getErrResult = fun (reason : RemoteReason') ->
            let reason : RemoteReason<'error> =
                match reason with
                | InvalidKind' kind ->
                    InvalidKind kind
                | RemoteNak' json ->
                    tryCastJson NakJson.JsonDecoder json
                    |> Result.mapError (fun err ->
                        sprintf "Stub.DecodeNak -> %s" err
                    )|> Result.get
                    |> fun nak -> (nak.Err, nak.Detail)
                    |> RemoteNak
                | RemoteError' json ->
                    tryCastJson errDecoder json
                    |> Result.mapError (fun err ->
                        sprintf "Stub.DecodeErr<%s> -> %s" errorType err
                    )|> Result.get
                    |> RemoteError
                | RemoteException' json ->
                    tryCastJson ExnJson.JsonDecoder json
                    |> Result.mapError (fun err ->
                        sprintf "Stub.DecodeExn -> %s" err
                    )|> Result.get
                    |> fun exn -> (exn.Msg, exn.Trace)
                    |> RemoteException
                | Timeout' seconds ->
                    Timeout seconds
            let res : StubResult<'result, 'error> =
                Error <| Remote reason
            res :> obj
        let getErrReason = fun (reason : LocalReason) ->
            let res : StubResult<'result, 'error> =
                Error <| Local reason
            res :> obj
        {
            Kind = kind
            Case = case
            ParamDecoder = FieldSpec.GetFieldsDecoder fields
            GetResResult = getResResult
            GetErrResult = getErrResult
            GetErrReason = getErrReason
        }

let inline spawnRes (spec : ResponseSpec<'res> list)
                        (req : IRequest)
                        (getResult : ResponseSpec<'res> -> obj) =
    spec
    |> List.find (fun s -> s.Kind = req.Kind)
    |> (fun spec ->
        let json = req.ToJson ()
        let param = json |> spec.ParamDecoder "" |> Result.get
        let result = getResult spec
        FSharpValue.MakeUnion(spec.Case, Array.append param [| result |]) :?> 'res
    )

type StubSpec<'req, 'res, 'evt> when 'evt :> IEvent = {
    Response : ResponseSpec<'res> list
    Event : CaseSpec<'evt> list
} with
    member this.DecodeResponse
            (
                runner : IRunner,
                req : IRequest,
                res : Result<Json, Reason'>
            #if FABLE_COMPILER
                , [<Inject>] ?resolver: ITypeResolver<'res>
            #endif
            ) : 'res =
        try
            match res with
            | Ok json ->
                spawnRes this.Response req (fun s -> s.GetResResult json)
            | Error reason ->
                match reason with
                | Local' reason ->
                    spawnRes this.Response req (fun s -> s.GetErrReason reason)
                | Remote' reason ->
                    spawnRes this.Response req (fun s -> s.GetErrResult reason)
        with e ->
        #if FABLE_COMPILER
            let resType = resolver.Value.ResolveType ()
        #else
            let resType = typeof<'res>
        #endif
            logException runner "Stub.DecodeResponse" resType.FullName (req, res) e
            raise e
    member this.DecodeEvent
            (
                runner : IRunner,
                json : Json
            #if FABLE_COMPILER
                , [<Inject>] ?resolver: ITypeResolver<'evt>
            #endif
            ) : 'evt =
        try
        #if FABLE_COMPILER
            castJson (D.union (this.Event, ?resolver=resolver)) json
        #else
            castJson (D.union this.Event) json
        #endif
        with e ->
        #if FABLE_COMPILER
            let evtType = resolver.Value.ResolveType ()
        #else
            let evtType = typeof<'evt>
        #endif
            logException runner "Stub.DecodeEvent" evtType.FullName json e
            raise e

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
        | RemoteError err -> ("Remote Error", err.EncodeJson 4, None)
        | RemoteException (msg, trace) -> ("Remote Exception", msg, Some trace)
        | Timeout seconds -> ("Timeout", sprintf "%A" seconds, None)

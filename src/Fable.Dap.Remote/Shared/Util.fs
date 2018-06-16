module Dap.Remote.Util

open Dap.Prelude
open Dap.Remote

#if FABLE_COMPILER
open Fable.Core
#endif

let checkKind (kind : string) (req : IRequest) : unit =
    if kind <> req.Kind then
        failwith <| sprintf "Kind_Not_Matched: %s -> %A" kind req

let decodeReason (decode : string -> Result<'err, string>) (reason : RemoteReason') : RemoteReason<'err> =
    match reason with
    | InvalidKind' kind -> InvalidKind kind
    | BadRequest' message -> BadRequest message
    | RemoteFailed' json -> RemoteFailed <| Result.get ^<| decode json
    | RemoteException' message -> RemoteException message
    | Timeout' seconds -> Timeout seconds

#if FABLE_COMPILER
let [<PassGenericsAttribute>] getReasonContent<'err when 'err :> IError> (reason : Reason<'err>) : string * string option =
#else
let getReasonContent (reason : Reason<'err> when 'err :> IError) : string * string option =
#endif
    match reason with
    | Local reason ->
        match reason with
        | EncodeFailed message -> ("Encode Failed", Some message)
        | DecodeFailed message -> ("Decode Failed", Some message)
        | SendFailed message -> ("Send Failed", Some message)
        | LocalException message -> ("Local Exception", Some message)
    | Remote reason ->
        match reason with
        | InvalidKind kind -> ("Invalid Kind", Some kind)
        | BadRequest message -> ("Bad Request", Some message)
        | RemoteFailed err -> (err.Payload, None)
        | RemoteException message -> ("Remote Exception", Some message)
        | Timeout seconds -> ("Timeout", Some <| sprintf "%As" seconds)

#if FABLE_COMPILER
#else
let forwardAck (onHandled : OnHandled) (res : Result<'res, 'err> when 'res :> IResponse and 'err :> IError) : unit =
    match res with
    | Ok res ->
        onHandled <| Ok (res :> IResponse)
    | Error err ->
        onHandled <| Error ^<| HubFailed (err :> IError)

let forwardNak (onHandled : OnHandled) ((err, detail) : string * obj) : unit =
    onHandled <| Error ^<| HubException err

#endif
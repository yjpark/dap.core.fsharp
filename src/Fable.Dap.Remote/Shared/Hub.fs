[<AutoOpen>]
[<RequireQualifiedAccess>]
module Dap.Remote.Hub

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

type HubReason =
    | HubNak of NakJson
    | HubError of IError
    | HubException of ExnJson

type Hub<'req, 'evt> = {
    PostReq : 'req -> unit
    OnEvent : IBus<'evt>
    OnDisconnected : unit -> unit
}

type OnHandled = Result<IResponse, HubReason> -> unit
type DecodeHubReq<'req> = PacketKind -> Json -> OnHandled -> 'req
type GetHub<'req, 'evt> = string -> (Hub<'req, 'evt> -> unit) -> unit

type HubSpec<'req, 'evt> = {
    DecodeReq : DecodeHubReq<'req>
    GetHub : GetHub<'req, 'evt>
}

type RequestSpec<'req> = {
    Case : UnionCaseInfo
    DecodeParam : Json -> obj
    GetCallback : IRunner -> OnHandled -> obj
} with
#if FABLE_COMPILER
    [<PassGenericsAttribute>]
#endif
    static member Create (kind : PacketKind) (paramDecoder : JsonDecoder<'param>)
                            getCallback : RequestSpec<'req> =
        let case =
            kind
            |> Union.tryFindCase<'req>
            |> Result.get
        let decodeParam = fun (json : Json) ->
            castJson paramDecoder json
            :> obj
        {
            Case = case
            DecodeParam = decodeParam
            GetCallback = getCallback
        }

let decodeReq (spec : RequestSpec<'req> list) (runner : IRunner) : DecodeHubReq<'req> =
    fun kind payload onHandled ->
        spec
        |> List.find (fun s -> s.Case.Name = kind)
        |> (fun spec ->
            let param = spec.DecodeParam payload
            let callback = spec.GetCallback runner onHandled
            FSharpValue.MakeUnion(spec.Case, [| param ; callback |]) :?> 'req
        )

#if FABLE_COMPILER
[<PassGenericsAttribute>]
#endif
let forwardAck<'res, 'err when 'res :> IResponse and 'err :> IError> (onHandled : OnHandled) (res : Result<'res, 'err>) : unit =
    match res with
    | Ok res ->
        onHandled <| Ok (res :> IResponse)
    | Error err ->
        onHandled <| Error ^<| HubError (err :> IError)

let forwardNak (onHandled : OnHandled) ((err, detail) : string * obj) : unit =
    onHandled <| Error ^<| HubNak ^<| NakJson.OfNak err detail

#if FABLE_COMPILER
[<PassGenericsAttribute>]
#endif
let getCallback<'res, 'err when 'res :> IResponse and 'err :> IError> (runner : IRunner) (onHandled : OnHandled) =
    callback' runner (forwardNak onHandled) (forwardAck<'res, 'err> onHandled)
    :> obj
[<AutoOpen>]
[<RequireQualifiedAccess>]
module Dap.Remote.Hub

open Microsoft.FSharp.Reflection
#if FABLE_COMPILER
open Fable.Core
#endif

open Dap.Prelude
open Dap.Context
open Dap.Platform
open Dap.Remote.Internal

type HubReason =
    | HubNak of NakJson
    | HubError of IError
    | HubException of ExnJson

type Hub<'req, 'evt> = {
    PostReq : 'req -> unit
    OnEvent : IBus<'evt>
}

type OnHandled = Result<IResult, HubReason> -> unit

type IGateway =
    inherit IAgent
    abstract Status : LinkStatus with get
    abstract OnStatus : IBus<LinkStatus>

type GetHub<'req, 'evt> = IGateway -> (Hub<'req, 'evt> -> unit) -> unit

type RequestSpec<'req> = {
    Case : UnionCaseInfo
    ParamDecoder : JsonDecoder<obj array>
    GetCallback : IRunner -> OnHandled -> obj
} with
#if FABLE_COMPILER
    [<PassGenericsAttribute>]
#endif
    static member Create (kind : PacketKind)
                            (fields : FieldSpec list)
                            getCallback : RequestSpec<'req> =
        let case = kind |> Union.findCase<'req>
        {
            Case = case
            ParamDecoder = FieldSpec.GetFieldsDecoder fields
            GetCallback = getCallback
        }

type HubSpec<'req, 'evt
            when 'req :> IReq and 'evt :> IEvt> = {
    Request : RequestSpec<'req> list
    GetHub : GetHub<'req, 'evt>
} with
#if FABLE_COMPILER
    [<PassGenericsAttribute>]
#endif
    member this.DecodeRequest (runner : IRunner) (json : Json) (onHandled : OnHandled) =
        try
            let kind = JsonKind.Cast json
            this.Request
            |> List.find (fun s -> s.Case.Name = kind.Value)
            |> (fun spec ->
#if FABLE_COMPILER
                let json = json :> obj
#endif
                let param = spec.ParamDecoder json |> Result.get
                let callback = spec.GetCallback runner onHandled
                FSharpValue.MakeUnion(spec.Case, Array.append param [| callback |]) :?> 'req
            )
        with e ->
            logException runner "Hub.DecodeRequest" typeof<'req>.FullName json e
            raise e

#if FABLE_COMPILER
[<PassGenericsAttribute>]
#endif
let forwardAck<'res, 'err when 'res :> IResult and 'err :> IError> (onHandled : OnHandled) (res : Result<'res, 'err>) : unit =
    match res with
    | Ok res ->
        onHandled <| Ok (res :> IResult)
    | Error err ->
        onHandled <| Error ^<| HubError (err :> IError)

let forwardNak (onHandled : OnHandled) ((err, detail) : string * obj) : unit =
    onHandled <| Error ^<| HubNak ^<| NakJson.OfNak err detail

#if FABLE_COMPILER
[<PassGenericsAttribute>]
#endif
let getCallback<'res, 'err when 'res :> IResult and 'err :> IError> (runner : IRunner) (onHandled : OnHandled) =
    callback' runner (forwardNak onHandled) (forwardAck<'res, 'err> onHandled)
    :> obj

#if !FABLE_COMPILER
let getHubSpec<'agent, 'req, 'evt when 'agent :> IAgent<'req, 'evt>> (kind : Kind) requestSpec (setGateway : IGateway -> 'agent -> unit) : HubSpec<'req, 'evt> =
    let getHub : GetHub<'req, 'evt> = fun gateway setHub ->
        gateway.Env.Handle <| DoGetAgent kind gateway.Ident.Key ^<| callback gateway (fun ((agent, isNew) : IAgent * bool) ->
            let agent = agent :?> 'agent
            if isNew then
                agent |> setGateway gateway
            let hub : Hub<'req, 'evt> = {
                PostReq = agent.Post
                OnEvent = agent.Actor.OnEvent
            }
            setHub hub
        )
    {
        Request = requestSpec
        GetHub = getHub
    }
#endif

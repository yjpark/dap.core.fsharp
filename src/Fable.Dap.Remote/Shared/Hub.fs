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
    static member Create
            (
                kind : PacketKind,
                fields : FieldSpec list,
                getCallback : IRunner -> OnHandled -> obj
            #if FABLE_COMPILER
                , [<Inject>] ?resolver: ITypeResolver<'req>
            #endif
            ) : RequestSpec<'req> =
    #if FABLE_COMPILER
        let case = Union.findCase<'req> (kind, ?resolver=resolver)
    #else
        let case = Union.findCase<'req> kind
    #endif
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
    member this.DecodeRequest (runner : IRunner) (json : Json) (onHandled : OnHandled) =
        try
            let kind = JsonKind.Cast json
            this.Request
            |> List.find (fun s -> s.Case.Name = kind.Value)
            |> (fun spec ->
                let param = spec.ParamDecoder "" json |> Result.get
                let callback = spec.GetCallback runner onHandled
                FSharpValue.MakeUnion(spec.Case, Array.append param [| callback |]) :?> 'req
            )
        with e ->
        #if FABLE_COMPILER
            let reqType = "'req"
        #else
            let reqType = typeof<'req>.FullName
        #endif
            logException runner "Hub.DecodeRequest" reqType json e
            raise e

let forwardAck<'res, 'err when 'res :> IResult and 'err :> IError> (onHandled : OnHandled) (res : Result<'res, 'err>) : unit =
    match res with
    | Ok res ->
        onHandled <| Ok (res :> IResult)
    | Error err ->
        onHandled <| Error ^<| HubError (err :> IError)

let forwardNak (onHandled : OnHandled) ((err, detail) : string * obj) : unit =
    onHandled <| Error ^<| HubNak ^<| NakJson.OfNak err detail

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

module Demo.UserHub.Types

open Dap.Prelude
open Dap.Context
open Dap.Platform
open Dap.Remote

type Req =
    | DoLogin of JsonString
with
    static member JsonSpec : CaseSpec<Req> list =
        [
            CaseSpec<Req>.Create ("DoLogin", [JsonString.JsonSpec])
        ]
    static member JsonDecoder = D.union Req.JsonSpec
    static member JsonEncoder = E.union Req.JsonSpec
    interface IJson with
        member this.ToJson () =
            Req.JsonEncoder this
    interface IRequest with
        member this.Kind = Union.getKind<Req> this

and Evt =
    | OnTick of JsonInt
with
    static member JsonSpec : CaseSpec<Evt> list =
        [
            CaseSpec<Evt>.Create ("OnTick", [JsonInt.JsonSpec])
        ]
    static member JsonDecoder = D.union Evt.JsonSpec
    static member JsonEncoder = E.union Evt.JsonSpec
    interface IJson with
        member this.ToJson () =
            Evt.JsonEncoder this
    interface IEvent with
        member this.Kind = Union.getKind<Evt> this

and ClientRes =
    | OnLogin of JsonString * StubResult<JsonBool, JsonString>
with
    static member StubSpec : Stub.ResponseSpec<ClientRes> list =
        [
            Stub.ResponseSpec<ClientRes>.Create ("DoLogin", [JsonString.JsonSpec],
                "OnLogin", JsonBool.JsonDecoder, JsonString.JsonDecoder)
        ]

type ServerReq =
    | DoLogin of JsonString * Callback<Result<JsonBool, JsonString>>
with
    static member HubSpec =
        [
            Hub.RequestSpec<ServerReq>.Create ("DoLogin", [JsonString.JsonSpec],
                Hub.getCallback<JsonBool, JsonString>)
        ]
    interface IReq

let StubSpec : StubSpec<Req, ClientRes, Evt> =
    {
        Response = ClientRes.StubSpec
        Event = Evt.JsonSpec
    }

[<AutoOpen>]
module Dap.Remote.Generator.Stub

open Dap.Prelude
open Dap.Context.Meta
open Dap.Context.Generator
open Dap.Context.Generator.Util
open Dap.Remote.Meta

type StubGenerator (meta : StubMeta) =
    let getFieldKind (field : StubFieldMeta) =
        sprintf "%s%s" (Union.getKind field.Kind) field.Type
        |> fun kind -> kind.Replace (".", "")
    let getResKind (req : StubFieldMeta) =
        match req.Kind with
        | Do -> "On"
        | To -> "By"
        | _ -> Union.getKind req.Kind
        |> fun kind -> sprintf "%s%s" kind req.Type
        |> fun kind -> kind.Replace (".", "")
    let getCaseMeta (key : string) (field : StubFieldMeta) =
        let kind = getFieldKind field
        let type' = sprintf "%s%s" field.Type key.AsCodeMemberName
        CaseMeta.Create kind [M.custom (type', key)]
    let getReq (_param : StubParam) =
        meta.Req
        |> List.map ^<| getCaseMeta "req"
        |> fun reqMeta ->
            G.JsonUnion ("Req", reqMeta)
            @ [
                "    interface IRequest with"
                "        member this.Kind = Union.getKind<Req> this"
            ]
    let getEvt (_param : StubParam) =
        meta.Evt
        |> List.map ^<| getCaseMeta "evt"
        |> fun evtMeta ->
            G.JsonUnion ("Evt", evtMeta)
            @ [
                "    interface IEvent with"
                "        member this.Kind = Union.getKind<Evt> this"
            ]
    let getClientRes (_param : StubParam) =
        [
            yield sprintf "type ClientRes ="
            for req in meta.Req do
                let resKind = getResKind req
                yield sprintf "    | %s of %sReq * StubResult<%sRes, %sErr>" resKind req.Type req.Type req.Type
            yield sprintf "with"
            yield sprintf "    static member StubSpec : Stub.ResponseSpec<ClientRes> list ="
            yield sprintf "        ["
            for req in meta.Req do
                let reqKind = getFieldKind req
                let resKind = getResKind req
                yield sprintf "            Stub.ResponseSpec<ClientRes>.Create (\"%s\", [%sReq.JsonSpec]," reqKind req.Type
                yield sprintf "                \"%s\", %sRes.JsonDecoder, %sErr.JsonDecoder)" resKind req.Type req.Type
            yield sprintf "        ]"
        ]
    let getServerReq (_param : StubParam) =
        [
            yield sprintf "type ServerReq ="
            for req in meta.Req do
                let reqKind = getFieldKind req
                yield sprintf "    | %s of %sReq * Callback<Result<%sRes, %sErr>>" reqKind req.Type req.Type req.Type
            yield sprintf "with"
            yield sprintf "    static member HubSpec : Hub.RequestSpec<ServerReq> list ="
            yield sprintf "        ["
            for req in meta.Req do
                let reqKind = getFieldKind req
                let resKind = getResKind req
                yield sprintf "            Hub.RequestSpec<ServerReq>.Create (\"%s\", [%sReq.JsonSpec]," reqKind req.Type
                yield sprintf "                Hub.getCallback<%sRes, %sErr>)" req.Type req.Type
            yield sprintf "        ]"
            yield sprintf "    interface IReq"
        ]
    interface IGenerator<StubParam> with
        member __.Generate param =
            [
                [
                    "open Dap.Platform"
                    "open Dap.Remote"
                    ""
                ]
                getReq param
                [""]
                getEvt param
                [""]
                getClientRes param
                [""]
                getServerReq param
                [
                    ""
                    "let StubSpec : StubSpec<Req, ClientRes, Evt> ="
                    "    {"
                    "        Response = ClientRes.StubSpec"
                    "        Event = Evt.JsonSpec'"
                    "    }"
                ]
            ]|> List.concat


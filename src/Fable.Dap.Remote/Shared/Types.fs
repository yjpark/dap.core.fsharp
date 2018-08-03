namespace Dap.Remote

open System

#if FABLE_COMPILER
module E = Thoth.Json.Encode
module D = Thoth.Json.Decode
#else
module E = Thoth.Json.Net.Encode
module D = Thoth.Json.Net.Decode
#endif

open Dap.Prelude
open Dap.Platform

type PacketId = string
type PacketKind = string

type IPayload =
    abstract Payload : Json with get

[<AutoOpen>]
module Extensions =
    type Json.E with
        static member json (payload : 'payload when 'payload :> IPayload) = (payload :> IPayload) .Payload

type IRequest =
    inherit IReq
    inherit IPayload
    abstract Kind : PacketKind with get

type IResponse = IPayload

type IError = IPayload

type IEvent =
    inherit IEvt
    inherit IPayload
    abstract Kind : PacketKind with get

type NoResponse = NoResponse
    with
        interface IResponse with
            member _this.Payload = E.nil

type NoError = NoError
    with
        interface IError with
            member _this.Payload = E.nil

type BoolPayload =
    BoolPayload of bool
with
    static member JsonDecoder =
        D.bool
        |> D.map (fun v -> BoolPayload v)
    member this.Value =
        let (BoolPayload v) = this
        v
    interface IPayload with
        member this.Payload = E.bool this.Value

type IntPayload =
    IntPayload of int
with
    static member JsonDecoder =
        D.int
        |> D.map (fun v -> IntPayload v)
    member this.Value =
        let (IntPayload v) = this
        v
    interface IPayload with
        member this.Payload = E.int this.Value

type StringPayload =
    StringPayload of string
with
    static member JsonDecoder =
        D.string
        |> D.map (fun v -> StringPayload v)
    member this.Value =
        let (StringPayload v) = this
        v
    interface IPayload with
        member this.Payload = E.string this.Value

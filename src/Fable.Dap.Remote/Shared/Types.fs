namespace Dap.Remote

open System

open Dap.Prelude
open Dap.Context
open Dap.Platform

type PacketId = string
type PacketKind = string

type IRequest =
    inherit IReq
    inherit IJson
    abstract Kind : String with get

type IResult = IJson

type IError = IJson

type IEvent =
    inherit IEvt
    inherit IJson
    abstract Kind : String with get

type NoResult = NoResult
with
    static member Create () = NoResult
    static member JsonEncoder (__ : NoResult) =
        E.nil
    static member JsonDecoder =
        D.nil NoResult
    static member JsonSpec = FieldSpec.Create<NoResult> (NoResult.JsonEncoder, NoResult.JsonDecoder)
    interface IResult with
        member this.ToJson () = NoResult.JsonEncoder this

type NoError = NoError
with
    static member Create () = NoError
    static member JsonEncoder (__ : NoError) =
        E.nil
    static member JsonDecoder =
        D.nil NoError
    static member JsonSpec = FieldSpec.Create<NoError> (NoError.JsonEncoder, NoError.JsonDecoder)
    interface IError with
        member this.ToJson () = NoError.JsonEncoder this

type JsonNil = JsonNil
with
    static member Create () = JsonNil
    static member JsonEncoder (__ : JsonNil) =
        E.nil
    static member JsonDecoder =
        D.nil JsonNil
    static member JsonSpec = FieldSpec.Create<JsonNil> (JsonNil.JsonEncoder, JsonNil.JsonDecoder)
    interface IJson with
        member this.ToJson () = JsonNil.JsonEncoder this

type JsonBool = JsonBool of bool
with
    static member Create (v) = JsonBool v
    member this.Value =
        let (JsonBool v) = this
        v
    static member JsonEncoder (this : JsonBool) =
        E.bool this.Value
    static member JsonDecoder =
        D.bool |> D.map (fun v -> JsonBool v)
    static member JsonSpec = FieldSpec.Create<JsonBool> (JsonBool.JsonEncoder, JsonBool.JsonDecoder)
    interface IJson with
        member this.ToJson () = JsonBool.JsonEncoder this

type JsonInt = JsonInt of int
with
    static member Create (v) = JsonInt v
    member this.Value =
        let (JsonInt v) = this
        v
    static member JsonEncoder (this : JsonInt) =
        E.int this.Value
    static member JsonDecoder =
        D.int |> D.map (fun v -> JsonInt v)
    static member JsonSpec = FieldSpec.Create<JsonInt> (JsonInt.JsonEncoder, JsonInt.JsonDecoder)
    interface IJson with
        member this.ToJson () = JsonInt.JsonEncoder this

type JsonString = JsonString of string
with
    static member Create (v) = JsonString v
    member this.Value =
        let (JsonString v) = this
        v
    static member JsonEncoder (this : JsonString) =
        E.string this.Value
    static member JsonDecoder =
        D.string |> D.map (fun v -> JsonString v)
    static member JsonSpec = FieldSpec.Create<JsonString> (JsonString.JsonEncoder, JsonString.JsonDecoder)
    interface IJson with
        member this.ToJson () = JsonString.JsonEncoder this

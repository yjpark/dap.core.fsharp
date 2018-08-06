namespace Dap.Remote

open System

open Dap.Prelude
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
    static member JsonEncoder (_this : NoResult) =
        E.nil
    static member JsonDecoder =
        D.nil NoResult
    interface IResult with
        member this.ToJson () = NoResult.JsonEncoder this

type NoError = NoError
with
    static member JsonEncoder (_this : NoError) =
        E.nil
    static member JsonDecoder =
        D.nil NoError
    interface IError with
        member this.ToJson () = NoError.JsonEncoder this

type JsonNil = JsonNil
with
    static member JsonEncoder (_this : JsonNil) =
        E.nil
    static member JsonDecoder =
        D.nil JsonNil
    interface IJson with
        member this.ToJson () = JsonNil.JsonEncoder this

type JsonBool = JsonBool of bool
with
    member this.Value =
        let (JsonBool v) = this
        v
    static member JsonEncoder (this : JsonBool) =
        E.bool this.Value
    static member JsonDecoder =
        D.bool |> D.map (fun v -> JsonBool v)
    interface IJson with
        member this.ToJson () = JsonBool.JsonEncoder this

type JsonInt = JsonInt of int
with
    member this.Value =
        let (JsonInt v) = this
        v
    static member JsonEncoder (this : JsonInt) =
        E.int this.Value
    static member JsonDecoder =
        D.int |> D.map (fun v -> JsonInt v)
    interface IJson with
        member this.ToJson () = JsonInt.JsonEncoder this

type JsonString = JsonString of string
with
    member this.Value =
        let (JsonString v) = this
        v
    static member JsonEncoder (this : JsonString) =
        E.string this.Value
    static member JsonDecoder =
        D.string |> D.map (fun v -> JsonString v)
    interface IJson with
        member this.ToJson () = JsonString.JsonEncoder this

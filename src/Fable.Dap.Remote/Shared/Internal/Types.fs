[<AutoOpen>]
module Dap.Remote.Internal.Types

open System

open Dap.Prelude
open Dap.Context
open Dap.Platform
open Dap.Remote

module Const =
    [<Literal>]
    let KindReq = "Req"

    [<Literal>]
    let KindRes = "Res"
    [<Literal>]
    let KindErr = "Err"

    [<Literal>]
    let KindNak = "Nak"

    [<Literal>]
    let KindExn = "Exn"

    [<Literal>]
    let KindEvt = "Evt"

[<StructuredFormatDisplay("<Agent>{AsDisplay}")>]
type Packet = {
    Time : DateTime
    Id : PacketId
    Kind : PacketKind
    Payload : Json
} with
    static member Create time id kind payload = {
        Time = time
        Id = id
        Kind = kind
        Payload = payload
    }
    static member JsonDecoder =
        D.decode Packet.Create
        |> D.required "t" D.dateTime
        |> D.required "i" D.string
        |> D.required "k" D.string
        |> D.required "p" D.json
    override this.ToString () =
        sprintf "[Packet: <%s> <%s> %s]" this.Id this.Kind <| String.capped 256 ^<| E.encode 0 this.Payload
    member this.AsDisplay = this.ToString ()
    interface IJson with
        member this.ToJson () =
            E.object [
                "t", E.dateTime this.Time
                "i", E.string this.Id
                "k", E.string this.Kind
                "p", this.Payload
            ]

type NakJson = {
    Err : string
    Detail : string
} with
    static member OfNak err (detail : obj) =
        if detail =? null then
            NakJson.Create err ""
        else
            NakJson.Create err <| detail.ToString ()
    static member Create err detail =
        {
            Err = err
            Detail = detail
        }
    static member JsonEncoder (this : NakJson) =
        E.object [
            "e", E.string this.Err
            "d", E.string this.Detail
        ]
    static member JsonDecoder =
        D.decode NakJson.Create
        |> D.required "e" D.string
        |> D.required "d" D.string
    interface IJson with
        member this.ToJson () = NakJson.JsonEncoder this

type ExnJson = {
    Msg : string
    Trace : string
} with
    static member OfExn (exn : Exception) =
        ExnJson.Create exn.Message exn.StackTrace
    static member Create msg trace =
        {
            Msg = msg
            Trace = trace
        }
    static member JsonEncoder (this : ExnJson) =
        E.object [
            "m", E.string this.Msg
            "t", E.string this.Trace
        ]
    static member JsonDecoder =
        D.decode ExnJson.Create
        |> D.required "m" D.string
        |> D.required "t" D.string
    interface IJson with
        member this.ToJson () = ExnJson.JsonEncoder this

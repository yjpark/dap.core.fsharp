[<AutoOpen>]
module Dap.Remote.Internal.Types

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
open Dap.Remote

module Const =
    [<Literal>]
    let KindUnknown = "N/A"

    [<Literal>]
    let KindAck = "Ack"

    [<Literal>]
    let KindNak = "Nak"

    [<Literal>]
    let KindErr = "Err"

    [<Literal>]
    let KindExn = "Exn"

    [<Literal>]
    let IdEvt = "Evt"

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
                "p", E.json this.Payload
            ]

type NakJson = {
    Err : string
    Detail : string
} with
    static member OfNak err (detail : obj) =
        NakJson.Create err <| detail.ToString ()
    static member Create err detail =
        {
            Err = err
            Detail = detail
        }
    static member JsonDecoder =
        D.decode NakJson.Create
        |> D.required "e" D.string
        |> D.required "d" D.string
    interface IJson with
        member this.ToJson () =
            E.object [
                "e", E.string this.Err
                "d", E.string this.Detail
            ]
    member this.Payload = E.json this
    interface IPayload with
        member this.Payload = this.Payload

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
    static member JsonDecoder =
        D.decode ExnJson.Create
        |> D.required "m" D.string
        |> D.required "t" D.string
    interface IJson with
        member this.ToJson () =
            E.object [
                "m", E.string this.Msg
                "t", E.string this.Trace
            ]
    member this.Payload = E.json this
    interface IPayload with
        member this.Payload = this.Payload


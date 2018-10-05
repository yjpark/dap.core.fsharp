[<AutoOpen>]
module Dap.Archive.Types

open System.IO
open Dap.Prelude
open Dap.Context
open Dap.Platform
open Dap.Remote
open Dap.Remote.Internal

type Meta<'extra> when 'extra :> IJson = {
    Kind : string
    Key : string
    Version : int
    Length : int
    Extra : 'extra
    BeginTime : Instant option
    EndTime : Instant option
    Memo : string option
} with
    static member Create kind key version length extra beginTime endTime memo = {
        Kind = kind
        Key = key
        Version = version
        Length = length
        Extra = extra
        BeginTime = beginTime
        EndTime = endTime
        Memo = memo
    }
    static member JsonDecoder (extraDecoder : JsonDecoder<'extra>) =
        D.object (fun get ->
            {
                Kind = get.Required.Field "kind" D.string
                Key = get.Required.Field "key" D.string
                Version = get.Required.Field "version" D.int
                Length = get.Required.Field "length" D.int
                Extra = get.Required.Field "extra" extraDecoder
                BeginTime = get.Optional.Field "begin_time" D.instant
                EndTime = get.Optional.Field "end_time" D.instant
                Memo = get.Optional.Field "memo" D.string
            }
        )
    interface IJson with
        member this.ToJson () =
            E.object [
                "kind", E.string this.Kind
                "key", E.string this.Key
                "version", E.int this.Version
                "length", E.int this.Length
                "extra", E.json this.Extra
                "begin_time", (E.option E.instant) this.BeginTime
                "end_time", (E.option E.instant) this.EndTime
                "memo", (E.option E.string) this.Memo
            ]

type IFrame =
    abstract Time : Instant with get
    abstract WriteTo : BinaryWriter -> unit

type ReadFrame<'frame> = BinaryReader -> Result<'frame, exn>

type PacketFrame = {
    Time : Instant
    Id : PacketId
    Kind : PacketKind
    Payload : string
}
with
    static member Create time id kind payload = {
        Time = time
        Id = id
        Kind = kind
        Payload = payload
    }
    static member OfPacket (pkt : Packet) =
        PacketFrame.Create (pkt.Time |> ofDateTimeUtc) pkt.Id pkt.Kind (pkt.Payload.EncodeJson 4)
    static member ReadFrom : ReadFrame<PacketFrame> =
        fun reader ->
            try
                let time = reader.ReadString ()
                instantOfText time
                |> Result.map (fun t ->
                    let i = reader.ReadString ()
                    let k = reader.ReadString ()
                    let p = reader.ReadString ()
                    let newline = reader.ReadString ()
                    if (newline <> "\n") then
                        failWith "Invalid_Newline" newline
                    PacketFrame.Create t i k p
                )
            with e ->
                Error e
    interface IFrame with
        member this.Time = this.Time
        member this.WriteTo writer =
            writer.Write (instantToText this.Time)
            writer.Write (this.Id)
            writer.Write (this.Kind)
            writer.Write (this.Payload)
            writer.Write ("\n")

type IStorage<'extra> when 'extra :> IJson =
    abstract OpenFramesStream : IRunner -> string -> Stream

type IStorage'<'extra> when 'extra :> IJson =
    abstract WriteMetaAsync : Meta<'extra> -> GetTask<IRunner, unit>
    abstract NewFramesStream : IRunner -> string -> Stream

let newMeta kind key version extra (beginTime : Instant) memo =
    {
        Kind = kind
        Key = key
        Version = version
        Length = 0
        Extra = extra
        BeginTime = Some beginTime
        EndTime = None
        Memo = memo
    }

let incLengthOfMeta (extra : 'extra) (meta : Meta<'extra>) =
    { meta with
        Length = meta.Length + 1
        Extra = extra
    }
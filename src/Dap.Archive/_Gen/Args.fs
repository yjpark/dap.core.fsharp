[<AutoOpen>]
module Dap.Archive.Args

open Dap.Prelude
open Dap.Context
open Dap.Platform

(*
 * Generated: <Record>
 *     IsJson, IsLoose
 *)
type RecorderArgs = {
    FlushInterval : (* RecorderArgs *) Duration
} with
    static member Create
        (
            ?flushInterval : (* RecorderArgs *) Duration
        ) : RecorderArgs =
        {
            FlushInterval = (* RecorderArgs *) flushInterval
                |> Option.defaultWith (fun () -> (decodeJsonString D.duration """0:00:00:30"""))
        }
    static member Default () =
        RecorderArgs.Create (
            (* RecorderArgs *) flushInterval = (decodeJsonString D.duration """0:00:00:30""")
        )
    static member SetFlushInterval ((* RecorderArgs *) flushInterval : Duration) (this : RecorderArgs) =
        {this with FlushInterval = flushInterval}
    static member JsonEncoder : JsonEncoder<RecorderArgs> =
        fun (this : RecorderArgs) ->
            E.object [
                "flush_interval", E.duration (* RecorderArgs *) this.FlushInterval
            ]
    static member JsonDecoder : JsonDecoder<RecorderArgs> =
        D.object (fun get ->
            {
                FlushInterval = get.Optional.Field (* RecorderArgs *) "flush_interval" D.duration
                    |> Option.defaultValue (decodeJsonString D.duration """0:00:00:30""")
            }
        )
    static member JsonSpec =
        FieldSpec.Create<RecorderArgs> (RecorderArgs.JsonEncoder, RecorderArgs.JsonDecoder)
    interface IJson with
        member this.ToJson () = RecorderArgs.JsonEncoder this
    interface IObj
    member this.WithFlushInterval ((* RecorderArgs *) flushInterval : Duration) =
        this |> RecorderArgs.SetFlushInterval flushInterval
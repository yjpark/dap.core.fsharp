[<AutoOpen>]
[<RequireQualifiedAccess>]
module Dap.Platform.Callback

open Dap.Prelude
open Dap.Platform

let wrap (wrapper : 'res -> 'a) (callback : Callback<'a>) : Callback<'res> =
    callback
    |> Option.map (fun callback ->
        fun (reply : Reply<'res>) ->
            match reply with
            | Ack (req, res) ->
                let res = wrapper res
                callback <| Ack (req, res)
            | Nak (req, err, detail) ->
                callback <| Nak (req, err, detail)
    )
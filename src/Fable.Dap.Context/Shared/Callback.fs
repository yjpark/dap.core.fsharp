[<AutoOpen>]
[<RequireQualifiedAccess>]
module Dap.Context.Callback

open Dap.Prelude

let wrap (wrapping : 'a -> 'b) (callback : Callback<'b>) : Callback<'a> =
    callback
    |> Option.map (fun callback ->
        fun (r : Reply<'a>) ->
            r
            |> Reply.map wrapping
            |> callback
    )

let forReq (req : IReq) (callback : Callback<'a>) : Callback<'a> =
    callback
    |> Option.map (fun callback ->
        fun (r : Reply<'a>) ->
            r
            |> Reply.forReq req
            |> callback
    )

let wrapForReq (wrapping : 'a -> 'b) (req : IReq) (callback : Callback<'b>) : Callback<'a> =
    callback
    |> Option.map (fun callback ->
        fun (r : Reply<'a>) ->
            r
            |> Reply.mapForReq wrapping req
            |> callback
    )

let get (r : Reply<'a>) : 'a =
    match r with
    | Ack (_req, res) ->
        res
    | Nak (_req, err, detail) ->
        failWith err <| sprintf "%A" detail

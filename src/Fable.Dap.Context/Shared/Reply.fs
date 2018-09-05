[<AutoOpen>]
[<RequireQualifiedAccess>]
module Dap.Context.Reply

open Dap.Prelude

let map (mapping : 'a -> 'b) (r : Reply<'a>) : Reply<'b> =
    match r with
    | Ack (req, res) ->
        Ack (req, mapping res)
    | Nak (req, err, detail) ->
        Nak (req, err, detail)

let forReq (req : IReq) (r : Reply<'a>) : Reply<'a> =
    match r with
    | Ack (_req, res) ->
        Ack (req, res)
    | Nak (_req, err, detail) ->
        Nak (req, err, detail)

let mapForReq (mapping : 'a -> 'b) (req : IReq) (r : Reply<'a>) : Reply<'b> =
    r
    |> map mapping
    |> forReq req

let get (r : Reply<'a>) : 'a =
    match r with
    | Ack (_req, res) ->
        res
    | Nak (_req, err, detail) ->
        failWith err <| sprintf "%A" detail

let toResult (r : Reply<'a>) : Result<'a, string * string> =
    match r with
    | Ack (_req, res) ->
        Ok res
    | Nak (_req, err, detail) ->
        Error (err, sprintf "%A" detail)
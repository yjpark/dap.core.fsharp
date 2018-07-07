[<AutoOpen>]
[<RequireQualifiedAccess>]
module Dap.Platform.Ticker.Service

open Dap.Platform

[<Literal>]
let Kind = "Ticker"

type Service = IAgent<Req, Evt>
type Args = Dap.Platform.Ticker.Types.Args

let addAsync' kind key args =
    Env.addServiceAsync (Logic.spec args) kind key

let get' kind key env =
    env |> Env.getService kind key :?> Service

let addAsync key = addAsync' Kind key
let get key = get' Kind key
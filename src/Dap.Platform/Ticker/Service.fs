[<AutoOpen>]
[<RequireQualifiedAccess>]
module Dap.Platform.Ticker.Service

open Dap.Platform
open Dap.Platform.Ticker.Types

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

let watchOnTick (owner : IOwner) (ident : string) onTick (ticker : Service) =
    ticker.Actor.OnEvent.AddWatcher owner ident (fun evt ->
        match evt with
        | OnTick (a, b) -> onTick (a, b)
        | _ -> ()
    )
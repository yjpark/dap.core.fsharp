[<AutoOpen>]
[<RequireQualifiedAccess>]
module Dap.Platform.Registry.Service

open Dap.Platform

[<Literal>]
let Kind = "Registry"

type Service<'k, 'v when 'k : comparison> = IAgent<Req<'k, 'v>, Evt<'k, 'v>>

let addAsync'<'k, 'v when 'k : comparison> kind key =
    Env.addServiceAsync Logic.spec<'k, 'v> kind key

let get'<'k, 'v when 'k : comparison> kind key env =
    env |> Env.getService kind key :?> Service<'k, 'v>

let addAsync<'k, 'v when 'k : comparison> key = addAsync'<'k, 'v> Kind key
let get<'k, 'v when 'k : comparison> key = get'<'k, 'v> Kind key

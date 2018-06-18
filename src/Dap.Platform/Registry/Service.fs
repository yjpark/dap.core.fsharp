[<AutoOpen>]
[<RequireQualifiedAccess>]
module Dap.Platform.Registry.Service

open Dap.Platform

[<Literal>]
let Kind = "Registry"

type Service<'k, 'v when 'k : comparison> = IAgent<Model<'k, 'v>, Req<'k, 'v>, Evt<'k, 'v>>

let add'<'k, 'v when 'k : comparison> (kind : Kind) (key : Key) =
    Logic.getSpec<'k, 'v>
    |> Env.addService kind key

let add<'k, 'v when 'k : comparison> (key : Key) =
    add'<'k, 'v> Kind key

let get'<'k, 'v when 'k : comparison> kind key (env : IEnv) =
    env
    |> Env.getService kind key
    :?> Service<'k, 'v>

let get<'k, 'v when 'k : comparison> key (env : IEnv) =
    get'<'k, 'v> Kind key env

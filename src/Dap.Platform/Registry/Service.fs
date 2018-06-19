[<AutoOpen>]
[<RequireQualifiedAccess>]
module Dap.Platform.Registry.Service

open FSharp.Control.Tasks
open Dap.Platform

[<Literal>]
let Kind = "Registry"

type Service<'k, 'v when 'k : comparison> = IAgent<Model<'k, 'v>, Req<'k, 'v>, Evt<'k, 'v>>

let addAsync'<'k, 'v when 'k : comparison> (kind : Kind) (key : Key) env = task {
    let! service = env |> Env.addServiceAsync kind key Logic.getSpec<'k, 'v>
    return service
}

let addAsync<'k, 'v when 'k : comparison> (key : Key) =
    addAsync'<'k, 'v> Kind key

let get'<'k, 'v when 'k : comparison> kind key (env : IEnv) =
    env
    |> Env.getService kind key
    :?> Service<'k, 'v>

let get<'k, 'v when 'k : comparison> key (env : IEnv) =
    get'<'k, 'v> Kind key env

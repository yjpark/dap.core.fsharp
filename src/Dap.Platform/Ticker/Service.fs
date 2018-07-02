[<AutoOpen>]
[<RequireQualifiedAccess>]
module Dap.Platform.Ticker.Service

open FSharp.Control.Tasks
open Dap.Platform

[<Literal>]
let Kind = "Ticker"

type Service = IAgent<Req, Evt>

let getSpec (autoStart : bool) (frameRate : double) =
    fun _runner ->
        {
            AutoStart = autoStart
            FrameRate = frameRate
        }
    |> Logic.getSpec

let addAsync' (kind : Kind) (key : Key) autoStart frameRate env = task {
    let spec = getSpec autoStart frameRate
    let! service = env |> Env.addServiceAsync kind key spec
    return service
}

let addAsync key frameRate =
    addAsync' Kind key true frameRate

let get' kind key (env : IEnv) =
    env
    |> Env.getService kind key
    :?> Service

let get key env =
    get' Kind key env
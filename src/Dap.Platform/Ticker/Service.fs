[<AutoOpen>]
[<RequireQualifiedAccess>]
module Dap.Platform.Ticker.Service

open Dap.Prelude
open Dap.Platform

[<Literal>]
let Kind = "Ticker"

type Service = IAgent<Model, Req, Evt>

let getSpec (autoStart : bool) (frameRate : int) =
    fun () ->
        {
            AutoStart = autoStart
            FrameRate = frameRate
            Event' = new Event<Evt>()
            InternalEvent' = new Event<InternalEvt>()
        }
    |> Logic.getSpec

let add' (kind : Kind) (key : Key) autoStart frameRate (env : IEnv) =
    getSpec autoStart frameRate
    |> Env.addService kind key

let add autoStart frameRate env =
    add' Kind "" autoStart frameRate env

let get' kind (env : IEnv) =
    env
    |> Env.getService kind
    :?> Service

let get env =
    get' Kind env
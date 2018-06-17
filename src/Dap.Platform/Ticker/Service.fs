[<AutoOpen>]
[<RequireQualifiedAccess>]
module Dap.Platform.Ticker.Service

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

let add autoStart key frameRate env =
    add' Kind key autoStart frameRate env

let get' kind key (env : IEnv) =
    env
    |> Env.getService kind key
    :?> Service

let get key env =
    get' Kind key env
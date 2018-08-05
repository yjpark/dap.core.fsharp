[<RequireQualifiedAccess>]
module Dap.Remote.WebSocketProxy.Proxy

open Fable.Core

open Dap.Platform
open Dap.Remote
open Dap.Remote.WebSocketProxy.Types

[<Literal>]
let Kind = "WebSocketProxy"

[<PassGenericsAttribute>]
let spawn'<'req, 'res, 'evt when 'req :> IRequest and 'evt :> IEvent>
            kind key args env =
    let spec = Logic.spec<'req, 'res, 'evt> args
    env |> Env.spawn spec kind key :?> IProxy<'req, 'res, 'evt>

[<PassGenericsAttribute>]
let spawn<'req, 'res, 'evt when 'req :> IRequest and 'evt :> IEvent> key = spawn'<'req, 'res, 'evt> Kind key

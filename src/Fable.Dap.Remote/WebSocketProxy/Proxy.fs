[<RequireQualifiedAccess>]
module Dap.Remote.WebSocketProxy.Proxy

open Dap.Platform
open Dap.Remote
open Dap.Remote.WebSocketProxy.Types

[<Literal>]
let Kind = "WebSocketProxy"

let spawn'<'req, 'res, 'evt when 'req :> IRequest and 'evt :> IEvent>
            kind key args env =
    let spec = Logic.spec<'req, 'res, 'evt> args
    env |> Env.spawn spec kind key :?> IProxy<'req, 'res, 'evt>

let spawn<'req, 'res, 'evt when 'req :> IRequest and 'evt :> IEvent> key = spawn'<'req, 'res, 'evt> Kind key

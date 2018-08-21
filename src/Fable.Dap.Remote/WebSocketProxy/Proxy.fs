[<RequireQualifiedAccess>]
module Dap.Remote.WebSocketProxy.Proxy

open Fable.Core

open Dap.Platform
open Dap.Remote
open Dap.Remote.Proxy.Types
open Dap.Remote.WebSocketProxy.Types

module Logic = Dap.Remote.WebSocketProxy.Logic
module BaseLogic = Dap.Remote.Proxy.Logic

[<Literal>]
let Kind = "WebSocketProxy"

[<PassGenericsAttribute>]
let spawn'<'req, 'res, 'evt when 'req :> IRequest and 'evt :> IEvent>
            kind key stubSpec uri logTraffic env =
    let subSpec : SubSpec<Extra, SubEvt, 'req, 'res, 'evt> = {
        NewExtra = Extra.New
        DoInit = Logic.doInit
        HandleSub = Logic.handleSub
        DoSend = Logic.doSend
    }
    let args = Args<Extra, SubEvt, 'req, 'res, 'evt>.Create subSpec stubSpec uri logTraffic
    let spec = BaseLogic.spec<Extra, SubEvt, 'req, 'res, 'evt> args
    env |> Env.spawn spec kind key :?> IProxy<'req, 'res, 'evt>

[<PassGenericsAttribute>]
let spawn<'req, 'res, 'evt when 'req :> IRequest and 'evt :> IEvent> key = spawn'<'req, 'res, 'evt> Kind key

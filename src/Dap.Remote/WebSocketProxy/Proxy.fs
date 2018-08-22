[<RequireQualifiedAccess>]
module Dap.Remote.WebSocketProxy.Proxy

open Dap.Platform
open Dap.Remote
open Dap.Remote.Proxy.Types
open Dap.Remote.WebSocketProxy.Types

module Logic = Dap.Remote.WebSocketProxy.Logic
module BaseLogic = Dap.Remote.Proxy.Logic

[<Literal>]
let Kind = "WebSocketProxy"

let spec (stubSpec : StubSpec<'req, 'res, 'evt>) uri retryDelay logTraffic =
    let subSpec : SubSpec<Extra, SubEvt, 'req, 'res, 'evt> = {
        NewExtra = Extra.New
        DoInit = Logic.doInit
        HandleSub = Logic.handleSub
        DoSend = Logic.doSend
    }
    Args<Extra, SubEvt, 'req, 'res, 'evt>.Create subSpec stubSpec uri retryDelay logTraffic
    |> BaseLogic.spec<Extra, SubEvt, 'req, 'res, 'evt>

let addAsync' kind key stubSpec uri retryDelay logTraffic =
    let spec = spec stubSpec uri retryDelay logTraffic
    Env.addServiceAsync spec kind key

let get'<'req, 'res, 'evt when 'req :> IRequest and 'evt :> IEvent> kind key env =
    env |> Env.getService kind key :?> IProxy<'req, 'res, 'evt>

let addAsync key = addAsync' Kind key
let get<'req, 'res, 'evt when 'req :> IRequest and 'evt :> IEvent> key = get' Kind key
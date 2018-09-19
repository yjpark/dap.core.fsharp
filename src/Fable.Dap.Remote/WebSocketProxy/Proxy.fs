[<RequireQualifiedAccess>]
module Dap.Remote.WebSocketProxy.Proxy

open Fable.Core

open Dap.Platform
open Dap.Context
open Dap.Remote
open Dap.Remote.Proxy.Types
open Dap.Remote.WebSocketProxy.Types

module BaseTypes = Dap.Remote.Proxy.Types
module BaseLogic = Dap.Remote.Proxy.Logic
module Logic = Dap.Remote.WebSocketProxy.Logic

[<Literal>]
let Kind = "WebSocketProxy"

type Args<'req, 'res, 'evt when 'req :> IRequest and 'evt :> IEvent> = BaseTypes.Args<Extra, SubEvt, 'req, 'res, 'evt>
type Proxy<'req, 'res, 'evt when 'req :> IRequest and 'evt :> IEvent> = BaseTypes.Proxy<Extra, SubEvt, 'req, 'res, 'evt>

[<PassGenericsAttribute>]
let args (stubSpec : Stub.StubSpec<'req, 'res, 'evt>) uri retryDelay logTraffic =
    let subSpec : BaseTypes.SubSpec<Extra, SubEvt, 'req, 'res, 'evt> = {
        NewExtra = Extra.New
        DoInit = Logic.doInit
        HandleSub = Logic.handleSub
        DoSend = Logic.doSend
    }
    Args<'req, 'res, 'evt>.Create subSpec stubSpec uri retryDelay logTraffic
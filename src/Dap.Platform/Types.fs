[<AutoOpen>]
module Dap.Platform.Types

open Dap.Prelude

[<Measure>]
type ms

type Scope = string
type Kind = string
type Key = string

type Ident = {
    Scope : Scope
    Kind : Kind
    Key : Key
}

type IMsg = interface end

type Response<'req, 'res> =
    | Ack of 'req * 'res
    | Nak of 'req * string * obj

type Reply<'res> = Response<IMsg, 'res>

exception ReplyException of string * obj

type Callback<'res> = (Reply<'res> -> unit) option

type IHandler<'req> =
    abstract Handle : 'req -> unit

type IPoster<'msg> =
    abstract Post : 'msg -> unit

and IChannel<'evt> =
    abstract OnEvent : IEvent<'evt> with get

and IActor =
    inherit ILogger
    abstract Ident : Ident with get

and IActor<'req, 'evt> =
    inherit IActor
    inherit IHandler<'req>
    inherit IChannel<'evt>

and IActor<'model, 'req, 'evt> =
    inherit IActor<'req, 'evt>
    abstract State : 'model option with get

and ActorSpec<'runner, 'args, 'model, 'msg, 'req, 'evt> = {
    Logic : Logic<'runner, 'args, 'model, 'msg>
    NewArgs : unit -> 'args
    WrapReq : Wrapper<'msg, 'req>
    GetOnEvent : 'model -> IEvent<'evt>
}

type NoArgs = NoArgs

and NoModel = NoModel

and NoMsg = NoMsg
with interface IMsg

and NoReq = NoReq

and NoEvt = NoEvt
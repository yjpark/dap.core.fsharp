[<AutoOpen>]
module Dap.Platform.Types

open Dap.Prelude

[<Measure>]
type second = Microsoft.FSharp.Data.UnitSystems.SI.UnitNames.second

[<Measure>]
type ms

let msPerSecond : float<ms/second> = 1000.0<ms/second>

type Scope = string
type Kind = string
type Key = string

[<Literal>]
let noScope = ""

[<Literal>]
let noKind = ""

[<Literal>]
let noKey = ""

let private calVersion scope kind key =
    let locker = obj()
    let mutable versions : Map<string, int> = Map.empty
    let ident = sprintf "%s:%s:%s" scope kind key 
    lock locker (fun () ->
        versions
        |> Map.tryFind ident
        |> function
            | None ->
                versions <- versions |> Map.add ident 1
                1
            | Some v ->
                versions <- versions |> Map.add ident (v + 1)
                (v + 1)
    )

[<StructuredFormatDisplay("{Ident}")>]
type Ident = {
    Scope : Scope
    Kind : Kind
    Key : Key
    Ver : int
} with
    static member Create scope kind key =
        {
            Scope = scope
            Kind = kind
            Key = key
            Ver = calVersion scope kind key
        }
    member this.Ident = sprintf "[%s:%s:%s]<%i>" this.Scope this.Kind this.Key this.Ver

let noIdent = 
    {
        Scope = noScope
        Kind = noKind
        Key = noKey
        Ver = 0
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
    abstract OnEvent : IBus<'evt> with get

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
    NewArgs : IOwner -> 'args
    WrapReq : Wrapper<'msg, 'req>
    GetOnEvent : 'model -> IBus<'evt>
}

type NoArgs = NoArgs

and NoModel = NoModel

and NoMsg = NoMsg
with interface IMsg

and NoReq = NoReq

and NoEvt = NoEvt
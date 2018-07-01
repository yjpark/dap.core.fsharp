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
    let mutable versions : Map<string, int> = Map.empty
    let ident = sprintf "%s:%s:%s" scope kind key
    let calc = fun () ->
        versions
        |> Map.tryFind ident
        |> function
            | None ->
                versions <- versions |> Map.add ident 1
                1
            | Some v ->
                versions <- versions |> Map.add ident (v + 1)
                (v + 1)
#if FABLE_COMPILER
    calc ()
#else
    let locker = obj()
    lock locker calc
#endif

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

[<StructuredFormatDisplay("<Agent>{AsDisplay}")>]
type ActorVersion = {
    StateVer : int
    MsgCount : int
} with
    override this.ToString () =
        sprintf "<S:%i M:%i>" this.StateVer this.MsgCount
    member this.AsDisplay = this.ToString ()


type IMsg = interface end
type IReq = interface end
type IEvt = interface end

type Reply<'res> =
    | Ack of IReq * 'res
    | Nak of IReq * string * obj

exception ReplyException of string * obj

type Callback<'res> = (Reply<'res> -> unit) option

type IHandler<'req> when 'req :> IReq =
    abstract Handle : 'req -> unit

type IPoster<'req> =
    abstract Post : 'req -> unit

and IChannel<'evt> when 'evt :> IEvt =
    abstract OnEvent : IBus<'evt> with get

and IActor =
    inherit ILogger
    abstract Ident : Ident with get

and IActor<'req, 'evt> when 'req :> IReq and 'evt :> IEvt =
    inherit IActor
    inherit IHandler<'req>
    inherit IChannel<'evt>

and IActor<'args, 'model, 'req, 'evt> when 'req :> IReq and 'evt :> IEvt =
    inherit IActor<'req, 'evt>
    abstract Args : 'args with get
    abstract State : 'model with get
    abstract Version : ActorVersion with get

and ActorSpec'<'owner, 'initer, 'runner, 'args, 'model, 'msg, 'req, 'evt> when 'msg :> IMsg and 'req :> IReq and 'evt :> IEvt = {
    Logic : Logic<'initer, 'runner, 'args, 'model, 'msg>
    NewArgs : NewArgs<'owner, 'args>
    WrapReq : Wrapper<'msg, 'req>
    CastEvt : CastEvt<'msg, 'evt>
}

and NewArgs<'owner, 'args> = 'owner -> 'args

and CastEvt<'msg, 'evt> = 'msg -> 'evt option

type NoArgs = NoArgs

and NoModel = NoModel

and NoMsg = NoMsg
with interface IMsg

and NoReq = NoReq
with interface IReq

and NoEvt = NoEvt
with interface IEvt
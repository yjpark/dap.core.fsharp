[<AutoOpen>]
module Dap.Platform.Types

#if !FABLE_COMPILER
open System.Threading.Tasks
#endif

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

[<StructuredFormatDisplay("{AsDisplay}")>]
type Version = {
    StateVer : int
    MsgCount : int
    ReqCount : int
    EvtCount : int
} with
    static member Init =
        {
            StateVer = 1
            MsgCount = 0
            ReqCount = 0
            EvtCount = 0
        }
    override this.ToString () =
        sprintf "<S:%i M:%i R:%i E:%i>" this.StateVer this.MsgCount this.ReqCount this.EvtCount
    member this.AsDisplay = this.ToString ()
    member this.IncMsg stateChanged =
        let stateVer = if stateChanged then this.StateVer + 1 else this.StateVer
        {this with StateVer = stateVer ; MsgCount = this.MsgCount + 1}
    member this.IncReq = {this with ReqCount = this.ReqCount + 1}
    member this.IncEvt = {this with EvtCount = this.EvtCount + 1}

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

#if !FABLE_COMPILER
type IAsyncHandler<'req> when 'req :> IReq =
    abstract HandleAsync<'res> : (Callback<'res> -> 'req) -> Task<'res>

type IAsyncPoster<'req> when 'req :> IReq =
    abstract PostAsync<'res> : (Callback<'res> -> 'req) -> Task<'res>
#endif

and IActor =
    abstract Ident : Ident with get

and IActor<'req, 'evt> when 'req :> IReq and 'evt :> IEvt =
    inherit IActor
    inherit IHandler<'req>
    inherit IChannel<'evt>
#if !FABLE_COMPILER
    inherit IAsyncHandler<'req>
#endif

and IActor<'args, 'model, 'req, 'evt> when 'req :> IReq and 'evt :> IEvt =
    inherit IActor<'req, 'evt>
    abstract Args : 'args with get
    abstract State : 'model with get
    abstract Version : Version with get

and IActorSpec<'msg, 'req, 'evt> when 'msg :> IMsg and 'req :> IReq and 'evt :> IEvt =
    abstract WrapReq : Wrapper<'msg, 'req> with get
    abstract CastEvt : CastEvt<'msg, 'evt> with get

and ActorSpec'<'owner, 'initer, 'runner, 'args, 'model, 'msg, 'req, 'evt> when 'msg :> IMsg and 'req :> IReq and 'evt :> IEvt = {
    Logic : Logic<'initer, 'runner, 'args, 'model, 'msg>
    NewArgs : NewArgs<'owner, 'args>
    WrapReq : Wrapper<'msg, 'req>
    CastEvt : CastEvt<'msg, 'evt>
} with
    interface IActorSpec<'msg, 'req, 'evt> with
        member this.WrapReq = this.WrapReq
        member this.CastEvt = this.CastEvt

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
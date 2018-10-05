[<AutoOpen>]
module Dap.Platform.Types

#if !FABLE_COMPILER
open System.Threading.Tasks
#endif
open Dap.Prelude
open Dap.Context

module ContextTypes = Dap.Context.Types

//Alias Context Types Begin
type Luid = ContextTypes.Luid
type Guid = ContextTypes.Guid

let newGuid () = ContextTypes.newGuid ()
let newLuid kind = ContextTypes.newLuid kind

type Kind = ContextTypes.Kind
type Key = ContextTypes.Key
type Index = ContextTypes.Index

type IOwner = ContextTypes.IOwner
type IBus<'evt> = ContextTypes.IBus<'evt>

type IReq = ContextTypes.IReq
type Reply<'res> = ContextTypes.Reply<'res>
type Callback<'res> = ContextTypes.Callback<'res>

//Alias Context Types End

[<Measure>]
type second = Microsoft.FSharp.Data.UnitSystems.SI.UnitNames.second

[<Measure>]
type ms

let msPerSecond : float<ms/second> = 1000.0<ms/second>

type Scope = string

[<Literal>]
let NoScope = ""

let private calVersion scope kind key =
    sprintf "%s:%s:%s" scope kind key
    |> newLuid'

[<StructuredFormatDisplay("{AsDisplay}")>]
type Ident = {
    Scope : Scope
    Kind : Kind
    Key : Key
    Ver : int
} with
    static member Create' scope kind key ver =
        {
            Scope = scope
            Kind = kind
            Key = key
            Ver = ver
        }
    static member Create scope kind key =
        calVersion scope kind key
        |> Ident.Create' scope kind key
    static member JsonEncoder : JsonEncoder<Ident> =
        fun (this : Ident) ->
            E.object [
                "scope", E.string this.Scope
                "kind", E.string this.Kind
                "key", E.string this.Key
                "ver", E.int this.Ver
            ]
    static member JsonDecoder : JsonDecoder<Ident> =
        D.object (fun get ->
            {
                Scope = get.Required.Field "scope" D.string
                Kind = get.Required.Field "kind" D.string
                Key = get.Required.Field "key" D.string
                Ver = get.Optional.Field "ver" D.int |> Option.defaultValue -1
            }
        )
    static member JsonSpec =
        FieldSpec.Create<Ident>
            Ident.JsonEncoder Ident.JsonDecoder
    interface IJson with
        member this.ToJson () = Ident.JsonEncoder this
    member this.ToLuid () = sprintf "[%s:%s:%s]<%i>" this.Scope this.Kind this.Key this.Ver
    override this.ToString () = this.ToLuid ()
    member this.AsDisplay = this.ToString ()

let noIdent =
    {
        Scope = NoScope
        Kind = NoKind
        Key = NoKey
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

type IEvt = interface end

type IMsg = interface end

type IPoster<'req> =
    abstract Post : 'req -> unit

#if !FABLE_COMPILER
type IAsyncPoster<'req> when 'req :> IReq =
    abstract PostAsync<'res> : (Callback<'res> -> 'req) -> Task<'res>
#endif

and IActor =
    abstract Ident : Ident with get

and IActor<'req, 'evt> when 'req :> IReq and 'evt :> IEvt =
    inherit IActor
    abstract Handle : 'req -> unit
    abstract OnEvent : IBus<'evt> with get
#if !FABLE_COMPILER
    abstract HandleAsync<'res> : (Callback<'res> -> 'req) -> Task<'res>
#endif
    abstract AsActor1 : IActor with get

and IActor<'args, 'model, 'req, 'evt> when 'req :> IReq and 'evt :> IEvt =
    inherit IActor<'req, 'evt>
    abstract Args : 'args with get
    abstract State : 'model with get
    abstract Version : Version with get
    abstract AsActor2 : IActor<'req, 'evt> with get

and CastEvt<'msg, 'evt> = 'msg -> 'evt option

and IActorSpec<'args, 'msg, 'req, 'evt> when 'msg :> IMsg and 'req :> IReq and 'evt :> IEvt =
    abstract Args : 'args with get
    abstract WrapReq : Wrapper<'msg, 'req> with get
    abstract CastEvt : CastEvt<'msg, 'evt> with get

type NoArgs = NoArgs
with
    static member JsonEncoder : JsonEncoder<NoArgs> =
        fun _this -> E.nil
    static member JsonDecoder : JsonDecoder<NoArgs> =
        D.succeed NoArgs
    static member JsonSpec =
        FieldSpec.Create<NoArgs> NoArgs.JsonEncoder NoArgs.JsonDecoder
    static member Default () = NoArgs
    interface IJson with
        member this.ToJson () = NoArgs.JsonEncoder this
    interface IObj

and NoModel = NoModel

and NoMsg = NoMsg
with interface IMsg

and NoReq = NoReq
with interface IReq

and NoEvt = NoEvt
with interface IEvt

type LinkStatus =
    | Unknown
    | NoLink
    | Linking
    | Linked
    | Closing
    | Closed
[<AutoOpen>]
module Dap.Context.Types

open System
#if FABLE_COMPILER
open Fable.Core
#else
open System.Threading.Tasks
#endif

open Dap.Prelude

type Luid = string  //Local Unique ID
type Guid = string  //Global Unique ID

let newGuid () : Guid =
    (System.Guid.NewGuid ()) .ToString ()

type Kind = string
type Key = string
type Index = int

let newLuid' kind =
    let mutable nexts : Map<string, int> = Map.empty
    let calc = fun () ->
        nexts
        |> Map.tryFind kind
        |> function
            | None ->
                nexts <- nexts |> Map.add kind 1
                1
            | Some v ->
                nexts <- nexts |> Map.add kind (v + 1)
                (v + 1)
#if FABLE_COMPILER
    calc ()
#else
    let locker = obj ()
    lock locker calc
#endif

let newLuid kind =
    newLuid' kind
    |> sprintf "%s:%d" kind

type IOwner =
    inherit ILogger
    abstract Luid : Luid with get
    abstract Disposed : bool with get

type Owner (logging' : ILogging, luid') =
    let logger = logging'.GetLogger luid'
    let luid : Luid = luid'
    let mutable disposed = false
    member _this.Dispose () =
        if not disposed then
            disposed <- true
            true
        else
            false
    member this.AsOwner = this :> IOwner
    interface IOwner with
        member _this.Log m = logger.Log m
        member _this.Luid = luid
        member _this.Disposed = disposed

[<Literal>]
let NoLuid = ""

[<Literal>]
let NoKind = ""

[<Literal>]
let NoKey = ""

let noOwner =
    let logger = getLogger "<noOwner>"
    { new IOwner with
        member _this.Log m = logger.Log m
        member _this.Luid = NoLuid
        member _this.Disposed = false
    }

type IBus<'msg> =
    abstract AddWatcher : IOwner -> Luid -> ('msg -> unit) -> unit
    abstract SetWatcher : IOwner -> Luid -> ('msg -> unit) -> bool    // -> isNew
    abstract RemoveWatcher' : IOwner -> Luid list                      // -> luid list
    abstract RemoveWatcher' : IOwner * Luid -> Luid list             // -> luid list
    abstract RemoveWatcher : IOwner -> unit
    abstract RemoveWatcher : IOwner * Luid -> unit

type IEvt = interface end

and IChannel<'evt> when 'evt :> IEvt =
    abstract OnEvent : IBus<'evt> with get

type IReq = interface end

type IHandler<'req> when 'req :> IReq =
    abstract Handle : 'req -> unit

//Note that can NOT add 'req into it, since it's used
// in Req definition.
type Reply<'res> =
    | Ack of IReq * 'res
    | Nak of IReq * string * obj

exception ReplyException of err : string * detail : obj
with
    override this.Message =
        sprintf "ReplyException: %s: %A" this.err this.detail

type Callback<'res> = (Reply<'res> -> unit) option

#if !FABLE_COMPILER
type IAsyncHandler<'req> when 'req :> IReq =
    abstract HandleAsync<'res> : (Callback<'res> -> 'req) -> Task<'res>
#endif

type IAspectSpec =
    abstract Luid : Luid with get
    abstract Key : Key with get

type IPropertySpec =
    inherit IAspectSpec
    abstract InitValue : Json with get

type PropertyChanged = {
    Spec : IPropertySpec
    Old : Json
    New : Json
}

and IProperty =
    inherit IJson
    abstract Ver : int with get
    abstract Spec : IPropertySpec with get
    abstract Seal : unit -> bool
    abstract Sealed : bool with get
    abstract WithJson : Json -> bool
    abstract OnChanged0 : IBus<PropertyChanged> with get

type IValue<'v> =
    abstract Value : 'v with get

type Validator<'v> = IValue<'v> -> 'v -> bool

type IPropertySpec<'v> =
    inherit IPropertySpec
    abstract Encoder : JsonEncoder<'v>
    abstract Decoder : JsonDecoder<'v>
    abstract InitValue : 'v with get
    abstract Validator : Validator<'v> option

type PropertyChanged<'v> = {
    Spec : IPropertySpec<'v>
    Old : 'v
    New : 'v
}

type IProperty<'v> =
    inherit IProperty
    inherit IValue<'v>
    abstract Spec : IPropertySpec<'v> with get
    abstract SetValue : 'v -> bool
    abstract OnChanged : IBus<PropertyChanged<'v>> with get

type IPropertyMap =
    inherit IProperty
    abstract Count : int with get
    abstract Has : Key -> bool
    abstract OnAdded0 : IBus<IProperty> with get
    abstract OnRemoved0 : IBus<IProperty> with get

type IPropertyMap<'v> =
    inherit IPropertyMap
    inherit IValue<Map<Luid, IProperty<'v>>>
    abstract Spec : IPropertySpec<'v> with get
    abstract TryGet : Key -> IProperty<'v> option
    abstract Get : Key -> IProperty<'v>
    abstract Set : Key -> 'v -> bool
    abstract Add : Key -> 'v -> IProperty<'v>
    abstract Remove : Key -> IProperty<'v> option
    abstract Clear : unit -> Map<Luid, IProperty<'v>>
    abstract OnAdded : IBus<IProperty<'v>> with get
    abstract OnRemoved : IBus<IProperty<'v>> with get
    abstract OnChanged : IBus<PropertyChanged<'v>> with get

type PropertyMoved = {
    Luid : Luid
    Old : Index
    New : Index
}

type ToIndex = int
type IndexOffset = int

type IPropertyList =
    inherit IProperty
    abstract Count : int with get
    abstract Has : Index -> bool
    abstract MoveTo : Index -> ToIndex -> unit
    abstract MoveBy : Index -> IndexOffset -> unit
    abstract Swap : Index -> Index -> unit
    abstract OnMoved : IBus<PropertyMoved> with get
    abstract OnAdded0 : IBus<IProperty * Index> with get
    abstract OnRemoved0 : IBus<IProperty * Index> with get

type IPropertyList<'v> =
    inherit IPropertyList
    inherit IValue<IProperty<'v> list>
    abstract Spec : IPropertySpec<'v> with get
    abstract TryGet : Index -> IProperty<'v> option
    abstract Get : Index -> IProperty<'v>
    abstract Set : Index -> 'v -> bool
    abstract Add : 'v -> IProperty<'v>
    abstract Insert : 'v -> ToIndex -> IProperty<'v>
    abstract Remove : Index -> IProperty<'v> option
    abstract Clear : unit -> IProperty<'v> list
    abstract OnAdded : IBus<IProperty<'v> * Index> with get
    abstract OnRemoved : IBus<IProperty<'v> * Index> with get
    abstract OnChanged : IBus<PropertyChanged<'v>> with get

type IProperties =
    inherit IProperty
    inherit IValue<Map<Key, IProperty>>
    abstract SealCombo : unit -> bool
    abstract ComboSealed : bool with get
    abstract TryGet : Key -> IProperty option
    abstract Has : Key -> bool
    abstract Get : Key -> IProperty
    abstract Add : IProperty -> unit
    abstract Add<'v> : IPropertySpec<'v> -> IProperty<'v>
    abstract AddMap<'v> : IPropertySpec<'v> -> IPropertyMap<'v>
    abstract AddList<'v> : IPropertySpec<'v> -> IPropertyList<'v>
    abstract OnAdded : IBus<IProperty> with get

type IContextSpec =
    abstract Kind : Kind with get
    abstract Luid : Luid with get
    abstract InitValue : Json with get

type IContext =
    inherit IOwner
    inherit IProperties
    abstract Dispose : unit -> bool
    abstract Properties : IProperties with get
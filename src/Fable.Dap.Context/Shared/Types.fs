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

type IObj = interface end

type IOwner =
    inherit IObj
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
    inherit IObj
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

and IValue<'v> =
    abstract Value : 'v with get

type Validator<'v> = IValue<'v> -> 'v -> bool

type IAspectSpec =
    abstract Luid : Luid with get
    abstract Key : Key with get

type IPropertySpec =
    inherit IAspectSpec
    abstract InitValue : Json with get

type PropertyChanged = {
    Spec : IAspectSpec
    Old : Json
    New : Json
}

type PropertyKind =
    | VarProperty
    | MapProperty
    | ListProperty
    | ComboProperty
    | CustomProperty

and PropertySpawner = IOwner -> Key -> IProperty

and IProperty =
    inherit IObj
    inherit IJson
    abstract Kind : PropertyKind with get
    abstract Ver : int with get
    abstract Spec : IPropertySpec with get
    abstract Seal : unit -> unit
    abstract Sealed : bool with get
    abstract WithJson : Json -> bool
    abstract OnChanged0 : IBus<PropertyChanged> with get
    abstract Clone0 : IOwner -> Key -> IProperty

and IPropertySpec<'v> =
    inherit IPropertySpec
    abstract Encoder : JsonEncoder<'v>
    abstract Decoder : JsonDecoder<'v>
    abstract InitValue : 'v with get
    abstract Validator : Validator<'v> option

and PropertyChanged<'v> = {
    Spec : IPropertySpec<'v>
    Old : 'v
    New : 'v
}

and PropertySpawner<'p when 'p :> IProperty> = IOwner -> Key -> 'p

and IVarProperty =
    inherit IProperty
    abstract ValueType : Type with get

and IVarProperty<'v> =
    inherit IVarProperty
    inherit IValue<'v>
    abstract Spec : IPropertySpec<'v> with get
    abstract SetValue : 'v -> bool
    abstract OnChanged : IBus<PropertyChanged<'v>> with get
    abstract Clone : IOwner -> Key -> IVarProperty<'v>

and IProperties =
    inherit IProperty
    abstract Clone1 : IOwner -> Key -> IProperties

and IMapProperty =
    inherit IProperties
    abstract ElementType : Type with get
    abstract Count : int with get
    abstract Has : Key -> bool
    abstract OnAdded0 : IBus<IProperty> with get
    abstract OnRemoved0 : IBus<IProperty> with get

and IMapProperty<'v> =
    inherit IMapProperty
    inherit IValue<Map<Luid, IVarProperty<'v>>>
    abstract Spec : IPropertySpec<'v> with get
    abstract TryGet : Key -> IVarProperty<'v> option
    abstract Get : Key -> IVarProperty<'v>
    abstract Set : Key -> 'v -> bool
    abstract Add : Key -> 'v -> IVarProperty<'v>
    abstract Remove : Key -> IVarProperty<'v> option
    abstract Clear : unit -> Map<Luid, IVarProperty<'v>>
    abstract OnAdded : IBus<IVarProperty<'v>> with get
    abstract OnRemoved : IBus<IVarProperty<'v>> with get
    abstract OnChanged : IBus<PropertyChanged<'v>> with get
    abstract Clone : IOwner -> Key -> IMapProperty<'v>

and PropertyMoved = {
    Luid : Luid
    Old : Index
    New : Index
}

and ToIndex = int
and IndexOffset = int

and IListProperty =
    inherit IProperties
    abstract ElementType : Type with get
    abstract Count : int with get
    abstract Has : Index -> bool
    abstract MoveTo : Index -> ToIndex -> unit
    abstract MoveBy : Index -> IndexOffset -> unit
    abstract Swap : Index -> Index -> unit
    abstract OnMoved : IBus<PropertyMoved> with get
    abstract OnAdded0 : IBus<IProperty * Index> with get
    abstract OnRemoved0 : IBus<IProperty * Index> with get

and IListProperty<'v> =
    inherit IListProperty
    inherit IValue<IVarProperty<'v> list>
    abstract Spec : IPropertySpec<'v> with get
    abstract TryGet : Index -> IVarProperty<'v> option
    abstract Get : Index -> IVarProperty<'v>
    abstract Set : Index -> 'v -> bool
    abstract Add : 'v -> IVarProperty<'v>
    abstract Insert : 'v -> ToIndex -> IVarProperty<'v>
    abstract Remove : Index -> IVarProperty<'v> option
    abstract Clear : unit -> IVarProperty<'v> list
    abstract OnAdded : IBus<IVarProperty<'v> * Index> with get
    abstract OnRemoved : IBus<IVarProperty<'v> * Index> with get
    abstract OnChanged : IBus<PropertyChanged<'v>> with get
    abstract Clone : IOwner -> Key -> IListProperty<'v>

and IComboProperty =
    inherit IProperties
    inherit IValue<Map<Key, IProperty>>
    abstract SealCombo : unit -> unit
    abstract ComboSealed : bool with get
    abstract TryGet : Key -> IProperty option
    abstract Has : Key -> bool
    abstract Get : Key -> IProperty
    abstract AddVar<'v> : IPropertySpec<'v> -> IVarProperty<'v>
    abstract AddMap<'v> : IPropertySpec<'v> -> IMapProperty<'v>
    abstract AddList<'v> : IPropertySpec<'v> -> IListProperty<'v>
    abstract AddCombo : IPropertySpec -> IComboProperty
    abstract AddCustom<'p when 'p :> IProperty> : Key -> (PropertySpawner<'p>) -> 'p
    abstract AddCustom0 : Key -> (PropertySpawner) -> IProperty
    abstract OnAdded : IBus<IProperty> with get
    abstract Clone : IOwner -> Key -> IComboProperty

and ICustomProperty =
    inherit IProperty

and ICustomProperty<'p when 'p :> ICustomProperty> =
    inherit ICustomProperty
    abstract Self : 'p with get
    abstract Clone : IOwner -> Key -> 'p

type IContextSpec =
    abstract Kind : Kind with get
    abstract Luid : Luid with get
    abstract PropertiesSpawner : IOwner -> IProperties

type IContext =
    inherit IOwner
    inherit IJson
    abstract Properties : IProperties with get
    abstract Dispose : unit -> bool
    abstract Clone0 : ILogging -> IContext
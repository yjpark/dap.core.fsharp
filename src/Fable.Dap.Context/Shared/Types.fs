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
    member __.Dispose () =
        if not disposed then
            disposed <- true
            true
        else
            false
    member this.AsOwner = this :> IOwner
    member __.Log m = logger.Log m
    member __.Luid = luid
    member __.Disposed = disposed
    interface IOwner with
        member __.Log m = logger.Log m
        member __.Luid = luid
        member __.Disposed = disposed

[<Literal>]
let NoLuid = ""

[<Literal>]
let NoKind = ""

[<Literal>]
let NoKey = ""

let noOwner = new Owner (getLogging (), "<noOwner>") :> IOwner

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
    Spec : IPropertySpec
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
and PropertySpawner<'p when 'p :> IProperty> = IOwner -> Key -> 'p

and IProperty =
    inherit IObj
    inherit IJson
    abstract Kind : PropertyKind with get
    abstract Ver : int with get
    abstract Spec : IPropertySpec with get
    abstract Seal : unit -> unit
    abstract Sealed : bool with get
    abstract WithJson : Json -> bool
    abstract OnChanged : IBus<PropertyChanged> with get
    abstract Clone0 : IOwner -> Key -> IProperty

and IVarPropertySpec<'v> =
    inherit IPropertySpec
    abstract Encoder : JsonEncoder<'v>
    abstract Decoder : JsonDecoder<'v>
    abstract InitValue : 'v with get
    abstract Validator : Validator<'v> option

and VarPropertyChanged<'v> = {
    Spec : IVarPropertySpec<'v>
    Old : 'v
    New : 'v
}

and IVarProperty =
    inherit IProperty
    abstract ValueType : Type with get

and IVarProperty<'v> =
    inherit IVarProperty
    inherit IValue<'v>
    abstract Spec : IVarPropertySpec<'v> with get
    abstract SetValue : 'v -> bool
    abstract OnValueChanged : IBus<VarPropertyChanged<'v>> with get
    abstract Clone : IOwner -> Key -> IVarProperty<'v>

and IPropertySpec<'p when 'p :> IProperty> =
    inherit IPropertySpec
    abstract Spawner : PropertySpawner<'p> with get

and IProperties =
    inherit IProperty
    abstract Count : int with get

and IMapProperty =
    inherit IProperties
    abstract ElementType : Type with get
    abstract ElementSpawner : IOwner -> Key -> IProperty
    abstract SealMap : unit -> unit
    abstract MapSealed : bool with get
    abstract Has : Key -> bool
    abstract OnAdded0 : IBus<IProperty> with get
    abstract OnRemoved0 : IBus<IProperty> with get

and IMapProperty<'p when 'p :> IProperty> =
    inherit IMapProperty
    inherit IValue<Map<Luid, 'p>>
    abstract Spec : IPropertySpec<'p> with get
    abstract TryGet : Key -> 'p option
    abstract Get : Key -> 'p
    abstract Add : Key -> 'p
    abstract Remove : Key -> 'p option
    abstract Clear : unit -> Map<Luid, 'p>
    abstract OnAdded : IBus<'p> with get
    abstract OnRemoved : IBus<'p> with get
    abstract Clone : IOwner -> Key -> IMapProperty<'p>

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
    abstract ElementSpawner : IOwner -> Key -> IProperty
    abstract SealList : unit -> unit
    abstract ListSealed : bool with get
    abstract Has : Index -> bool
    abstract MoveTo : Index -> ToIndex -> unit
    abstract MoveBy : Index -> IndexOffset -> unit
    abstract Swap : Index -> Index -> unit
    abstract OnMoved : IBus<PropertyMoved> with get
    abstract OnAdded0 : IBus<IProperty * Index> with get
    abstract OnRemoved0 : IBus<IProperty * Index> with get

and IListProperty<'p when 'p :> IProperty> =
    inherit IListProperty
    inherit IValue<'p list>
    abstract Spec : IPropertySpec<'p> with get
    abstract TryGet : Index -> 'p option
    abstract Get : Index -> 'p
    abstract Add : unit -> 'p
    abstract Insert : ToIndex -> 'p
    abstract Remove : Index -> 'p option
    abstract Clear : unit -> 'p list
    abstract OnAdded : IBus<'p * Index> with get
    abstract OnRemoved : IBus<'p * Index> with get
    abstract Clone : IOwner -> Key -> IListProperty<'p>

and IComboProperty =
    inherit IProperties
    inherit IValue<Map<Key, IProperty>>
    abstract SealCombo : unit -> unit
    abstract ComboSealed : bool with get
    abstract TryGet : Key -> IProperty option
    abstract Has : Key -> bool
    abstract Get : Key -> IProperty
    abstract AddAny : Key -> PropertySpawner -> IProperty
    abstract AddVar<'v> : IVarPropertySpec<'v> -> IVarProperty<'v>
    abstract AddMap<'p when 'p :> IProperty> : IPropertySpec<'p> -> IMapProperty<'p>
    abstract AddList<'p when 'p :> IProperty> : IPropertySpec<'p> -> IListProperty<'p>
    abstract AddCombo : IPropertySpec -> IComboProperty
    abstract AddCustom<'p when 'p :> ICustomProperty> : IPropertySpec<'p> -> 'p
    abstract OnAdded : IBus<IProperty> with get
    abstract SyncTo : IComboProperty -> unit
    abstract Clone : IOwner -> Key -> IComboProperty

and ICustomProperty =
    inherit IProperty

and ICustomProperties =
    inherit ICustomProperty
    inherit IProperties

and ICustomProperty<'p when 'p :> ICustomProperty> =
    inherit ICustomProperty
    abstract Self : 'p with get
    abstract SyncTo : 'p -> unit
    abstract Clone : IOwner -> Key -> 'p

type IContextSpec =
    abstract Kind : Kind with get
    abstract Luid : Luid with get

type IContextSpec<'p when 'p :> IProperties> =
    inherit IContextSpec
    abstract SpawnProperties : IOwner -> 'p

type IContext =
    inherit IOwner
    inherit IJson
    abstract Dispose : unit -> bool
    abstract Spec0 : IContextSpec with get
    abstract Properties0 : IProperties with get
    abstract Clone0 : ILogging -> IContext

type IContext<'p when 'p :> IProperties> =
    inherit IContext
    abstract Properties : 'p with get

type IContext<'c, 's, 'p when 'c :> IContext and 's :> IContextSpec and 'p :> IProperties> =
    inherit IContext<'p>
    abstract Self : 'c with get
    abstract Spec : 's with get
    abstract Clone : ILogging -> 'c

[<AutoOpen>]
module Extensions =
    type IOwner with
        static member Create (logging, luid) =
            new Owner (logging, luid)
            :> IOwner
    type IProperty with
        member this.SyncTo0 (other : IProperty) =
            this.ToJson ()
            |> other.WithJson
            |> ignore
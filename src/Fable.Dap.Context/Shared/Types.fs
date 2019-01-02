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

let newLuid () : Guid =
    (System.Guid.NewGuid ()) .ToString ()

type Kind = string
type Key = string
type Index = int

let private newLuidOfKindLock = obj ()

let newLuidOfKind' kind =
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
    lock newLuidOfKindLock calc
#endif

let newLuidOfKind kind =
    newLuidOfKind' kind
    |> sprintf "%s:%d" kind

type IObj = interface end

type IOwner =
    inherit IObj
    inherit ILogger
    abstract Luid : Luid with get
    abstract Disposed : bool with get

type Owner internal (logging' : ILogging, luid : Luid) =
    let logger = logging'.GetLogger luid
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

[<Literal>]
let PropertiesKey = "_P_"

[<Literal>]
let ChannelsKey = "_C_"

[<Literal>]
let HandlersKey = "_H_"

#if !FABLE_COMPILER
[<Literal>]
let AsyncHandlersKey = "_A_"
#endif

let noOwner = new Owner (getLogging (), "<noOwner>") :> IOwner

type IBus<'msg> =
    inherit IObj
    abstract Owner : IOwner with get
    abstract Ver : int with get
    abstract AddWatcher : IOwner -> Luid -> ('msg -> unit) -> unit
    abstract SetWatcher : IOwner -> Luid -> ('msg -> unit) -> bool    // -> isNew
    abstract RemoveWatcher' : IOwner -> Luid list                      // -> luid list
    abstract RemoveWatcher' : IOwner * Luid -> Luid list             // -> luid list
    abstract RemoveWatcher : IOwner -> unit
    abstract RemoveWatcher : IOwner * Luid -> unit

and IValue<'v> =
    abstract Value : 'v with get

type Validator<'v> = {
    Check : IValue<'v> -> 'v -> bool
}

type IAspectSpec =
    abstract Luid : Luid with get
    abstract Key : Key with get

type IAspect =
    inherit IObj
    abstract Owner : IOwner with get
    abstract Ver : int with get
    abstract SpecA : IAspectSpec with get

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
    | DictProperty
    | ListProperty
    | ComboProperty
    | CustomProperty

//If use curried version here, fable will have issue
and PropertySpawner = IOwner * Key -> IProperty
and PropertySpawner<'p when 'p :> IProperty> = IOwner * Key -> 'p

and IProperty =
    inherit IAspect
    inherit IJson
    abstract Kind : PropertyKind with get
    abstract Spec0 : IPropertySpec with get
    abstract Seal : unit -> unit
    abstract Sealed : bool with get
    abstract LoadJson' : Json -> bool
    abstract OnChanged0 : IBus<PropertyChanged> with get
    abstract Clone0 : IOwner * Key -> IProperty
    abstract SyncTo0 : IProperty -> unit

and IPropertySpec<'p when 'p :> IProperty> =
    inherit IPropertySpec
    abstract Spawner : PropertySpawner<'p> with get

and IVarPropertySpec<'v> =
    inherit IPropertySpec<IVarProperty<'v>>
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
#if !FABLE_COMPILER
    abstract ValueType : Type with get
#endif

and IVarProperty<'v> =
    inherit IVarProperty
    inherit IValue<'v>
    abstract Spec : IVarPropertySpec<'v> with get
    abstract SetValue' : 'v -> bool
    abstract OnChanged : IBus<VarPropertyChanged<'v>> with get
    abstract SyncTo : IVarProperty<'v> -> unit
    abstract Clone : IOwner * Key -> IVarProperty<'v>

and IProperties =
    inherit IProperty
    abstract Count : int with get

and IDictProperty =
    inherit IProperties
#if !FABLE_COMPILER
    abstract ElementType : Type with get
#endif
    abstract ElementSpawner : IOwner * Key -> IProperty
    abstract SealDict : unit -> unit
    abstract DictSealed : bool with get
    abstract Has : Key -> bool
    abstract OnAdded0 : IBus<IProperty> with get
    abstract OnRemoved0 : IBus<IProperty> with get

and IDictProperty<'p when 'p :> IProperty> =
    inherit IDictProperty
    inherit IValue<Map<Key, 'p>>
    abstract Spec : IPropertySpec<'p> with get
    abstract TryGet : Key -> 'p option
    abstract Get : Key -> 'p
    abstract Add : Key -> 'p
    abstract Remove : Key -> 'p option
    abstract Clear' : unit -> Map<Key, 'p>
    abstract OnAdded : IBus<'p> with get
    abstract OnRemoved : IBus<'p> with get
    abstract SyncTo : IDictProperty<'p> -> unit
    abstract Clone : IOwner * Key -> IDictProperty<'p>

and PropertyMoved = {
    Spec : IPropertySpec
    Old : Index
    New : Index
}

and ToIndex = int
and IndexOffset = int

and IListProperty =
    inherit IProperties
#if !FABLE_COMPILER
    abstract ElementType : Type with get
#endif
    abstract ElementSpawner : IOwner * Key -> IProperty
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
    abstract Remove : Index -> 'p
    abstract Clear' : unit -> 'p list
    abstract OnAdded : IBus<'p * Index> with get
    abstract OnRemoved : IBus<'p * Index> with get
    abstract SyncTo : IListProperty<'p> -> unit
    abstract Clone : IOwner * Key -> IListProperty<'p>

and IComboProperty =
    inherit IProperties
    inherit IValue<IProperty list>
    abstract SealCombo : unit -> unit
    abstract ComboSealed : bool with get
    abstract TryGet : Key -> IProperty option
    abstract Has : Key -> bool
    abstract Get : Key -> IProperty
    abstract AddAny : Key -> PropertySpawner -> IProperty
    abstract AddVar<'v> : IVarPropertySpec<'v> -> IVarProperty<'v>
    abstract AddDict<'p when 'p :> IProperty> : IPropertySpec<'p> -> IDictProperty<'p>
    abstract AddList<'p when 'p :> IProperty> : IPropertySpec<'p> -> IListProperty<'p>
    abstract AddCombo : IPropertySpec -> IComboProperty
    abstract AddCustom<'p when 'p :> ICustomProperty> : IPropertySpec<'p> -> 'p
    abstract OnAdded : IBus<IProperty> with get
    abstract SyncTo : IComboProperty -> unit
    abstract Clone : IOwner * Key -> IComboProperty

and ICustomProperty =
    inherit IProperty

and ICustomProperties =
    inherit ICustomProperty
    inherit IProperties

and ICustomProperty<'p when 'p :> ICustomProperty> =
    inherit ICustomProperty
    abstract Self : 'p with get
    abstract SyncTo : 'p -> unit
    abstract Clone : IOwner * Key -> 'p

type IChannelSpec =
    inherit IAspectSpec
#if !FABLE_COMPILER
    abstract EvtType : Type with get
#endif

and IChannelSpec<'evt> =
    inherit IChannelSpec
    abstract Encoder : JsonEncoder<'evt>
    abstract Decoder : JsonDecoder<'evt>

type EventFired = {
    Spec : IChannelSpec
    Evt : Json
}

and IChannel =
    inherit IAspect
    abstract Spec0 : IChannelSpec with get
    abstract Muted : bool with get
    abstract SetMuted : bool -> unit
    abstract FireEvent0 : Json -> unit
    abstract OnEvent0 : IBus<EventFired> with get

type EventFired<'evt> = {
    Spec : IChannelSpec<'evt>
    Evt : 'evt
}

and IChannel<'evt> =
    inherit IChannel
    abstract Spec : IChannelSpec<'evt> with get
    abstract FireEvent : 'evt -> unit
    abstract OnEvent : IBus<EventFired<'evt>> with get

and IChannels =
    inherit IAspect
    inherit IValue<IChannel list>
    abstract Seal : unit -> unit
    abstract Sealed : bool with get
    abstract TryGet : Key -> IChannel option
    abstract Has : Key -> bool
    abstract Get : Key -> IChannel
    abstract Add<'evt> : IChannelSpec<'evt> -> IChannel<'evt>
    abstract OnAdded : IBus<IChannel> with get

type IHandlerSpec =
    inherit IAspectSpec
#if !FABLE_COMPILER
    abstract ReqType : Type with get
    abstract ResType : Type with get
#endif

and IHandlerSpec<'req, 'res> =
    inherit IHandlerSpec
    abstract ReqEncoder : JsonEncoder<'req>
    abstract ReqDecoder : JsonDecoder<'req>
    abstract ResEncoder : JsonEncoder<'res>
    abstract ResDecoder : JsonDecoder<'res>

type RequestReceived = {
    Spec : IHandlerSpec
    Req : Json
}

type RequestHandled = {
    Spec : IHandlerSpec
    Req : Json
    Res : Json
}

type IHandler =
    inherit IAspect
    abstract Spec0 : IHandlerSpec with get
    abstract Seal : unit -> unit
    abstract Sealed : bool with get
    abstract Muted : bool with get
    abstract SetMuted : bool -> unit
    abstract Handle0 : Json -> Json
    abstract OnRequest0 : IBus<RequestReceived> with get
    abstract OnResponse0 : IBus<RequestHandled> with get

type RequestReceived<'req, 'res> = {
    Spec : IHandlerSpec<'req, 'res>
    Req : 'req
}

type RequestHandled<'req, 'res> = {
    Spec : IHandlerSpec<'req, 'res>
    Req : 'req
    Res : 'res
}

type IHandler<'req, 'res> =
    inherit IHandler
    abstract Spec : IHandlerSpec<'req, 'res> with get
    abstract SetupHandler' : ('req -> 'res) -> unit
    abstract Handle : 'req -> 'res
    abstract OnRequest : IBus<RequestReceived<'req, 'res>> with get
    abstract OnResponse : IBus<RequestHandled<'req, 'res>> with get

and IHandlers =
    inherit IAspect
    inherit IValue<IHandler list>
    abstract Seal : unit -> unit
    abstract Sealed : bool with get
    abstract TryGet : Key -> IHandler option
    abstract Has : Key -> bool
    abstract Get : Key -> IHandler
    abstract Add<'req, 'res> : IHandlerSpec<'req, 'res> -> IHandler<'req, 'res>
    abstract OnAdded : IBus<IHandler> with get

#if !FABLE_COMPILER
type IAsyncHandler =
    inherit IAspect
    abstract Spec0 : IHandlerSpec with get
    abstract Seal : unit -> unit
    abstract Sealed : bool with get
    abstract Muted : bool with get
    abstract SetMuted : bool -> unit
    abstract Handle0 : Json -> Task<Json>
    abstract OnRequest0 : IBus<RequestReceived> with get
    abstract OnResponse0 : IBus<RequestHandled> with get

type IAsyncHandler<'req, 'res> =
    inherit IAsyncHandler
    abstract Spec : IHandlerSpec<'req, 'res> with get
    abstract SetupHandler' : ('req -> Task<'res>) -> unit
    abstract Handle : 'req -> Task<'res>
    abstract OnRequest : IBus<RequestReceived<'req, 'res>> with get
    abstract OnResponse : IBus<RequestHandled<'req, 'res>> with get

and IAsyncHandlers =
    inherit IAspect
    inherit IValue<IAsyncHandler list>
    abstract Seal : unit -> unit
    abstract Sealed : bool with get
    abstract TryGet : Key -> IAsyncHandler option
    abstract Has : Key -> bool
    abstract Get : Key -> IAsyncHandler
    abstract Add<'req, 'res> : IHandlerSpec<'req, 'res> -> IAsyncHandler<'req, 'res>
    abstract OnAdded : IBus<IAsyncHandler> with get
#endif

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
    abstract Channels : IChannels with get
    abstract Handlers : IHandlers with get
#if !FABLE_COMPILER
    abstract AsyncHandlers : IAsyncHandlers with get
#endif
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
        static member Create (luid) =
            IOwner.Create (getLogging(), luid)
    type IAspect with
        member this.Luid = this.SpecA.Luid
        member this.Key = this.SpecA.Key
    type IProperty with
        member this.LoadJson (json : Json) =
            this.LoadJson' json |> ignore
        member this.SyncWith0 (other : IProperty) =
            other.SyncTo0 this
    type IComboProperty with
        member this.Get<'p when 'p :> IProperty> (key : Key) =
            this.Get key
            :?> 'p
        member this.SyncWith (other : IComboProperty) =
            other.SyncTo this
    type IVarProperty<'v> with
        member this.SetValue (v : 'v) =
            this.SetValue' v
            |> function
                | true -> ()
                | false ->
                    logError this.Owner "IVarProperty<_>" "SetValue_Failed" (this.Value, v)
        member this.SyncWith (other : IVarProperty<'v>) =
            other.SyncTo this
    type IDictProperty<'p when 'p :> IProperty> with
        member this.SyncWith (other : IDictProperty<'p>) =
            other.SyncTo this
        member this.GetOrAdd (key : Key) =
            match this.TryGet key with
            | Some p -> p
            | None -> this.Add key
        member this.Clear () =
            this.Clear' () |> ignore
    type IListProperty<'p when 'p :> IProperty> with
        member this.SyncWith (other : IListProperty<'p>) =
            other.SyncTo this
        member this.Clear () =
            this.Clear' () |> ignore
    type ICustomProperty<'p when 'p :> ICustomProperty> with
        member this.SyncWith (other : ICustomProperty<'p>) =
            other.SyncTo this.Self
    type IHandler<'req, 'res> with
        member this.SetupHandler (handler : 'req -> 'res) =
            this.SetupHandler' handler
            this.Seal ()
#if !FABLE_COMPILER
    type IAsyncHandler<'req, 'res> with
        member this.SetupHandler (handler : 'req -> Task<'res>) =
            this.SetupHandler' handler
            this.Seal ()
#endif

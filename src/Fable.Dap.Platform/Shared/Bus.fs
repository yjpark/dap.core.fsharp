[<AutoOpen>]
module Dap.Platform.Bus

open System
open Dap.Prelude

// This is to provide some extra feature to Event<> and IEvent<>
//
// Related Readings:
// - https://www.codeproject.com/Articles/29922/Weak-Events-in-C
// - https://sachabarbs.wordpress.com/2014/04/23/f-21-events/ 

type IOwner =
    inherit ILogger
    abstract Ident : string with get
    abstract Disposed : bool with get

let private tplBusError = LogEvent.Template2WithException<string, obj>(LogLevelError, "[{Section}] {Msg} -> Failed")

let private tplDisposedInfo = LogEvent.Template3<string, obj, obj>(LogLevelInformation, "[{Section}] {Owner} {Detail}")

let private tplWatchersInfo = LogEvent.Template3<string, obj, obj>(LogLevelInformation, "[{Section}] {Watchers} {Detail}")

let private tplWatchersDebug = LogEvent.Template3<string, obj, obj>(LogLevelDebug, "[{Section}] {Watchers} {Detail}")

let private tplWatcherSucceed = LogEvent.Template3<string, obj, obj>(LogLevelDebug, "[{Section}] {Watcher} {Detail}")

let private tplWatcherInfo = LogEvent.Template3<string, obj, obj>(LogLevelInformation, "[{Section}] {Watcher} {Detail}")

let private tplWatcherDebug = LogEvent.Template3<string, obj, obj>(LogLevelDebug, "[{Section}] {Watcher} {Detail}")

let private tplWatcherFailed = LogEvent.Template3WithException<string, obj, obj>(LogLevelError, "[{Section}] {Watcher} {Detail} -> Failed")

let private tplAddWatcherError = LogEvent.Template5<string, IOwner, string, obj, obj>(LogLevelError, "[{Section}] {Owner} {Ident} {Action} -> {Old}")

type Watcher<'evt> = {
    OwnerIdent : string
#if FABLE_COMPILER
    OwnerRef : IOwner
#else
    OwnerRef : WeakReference<IOwner>
#endif
    Ident : string
    Action : 'evt -> unit
} with
    static member Create (owner : IOwner) ident action =
        {
            OwnerIdent = owner.Ident
        #if FABLE_COMPILER
            OwnerRef = owner
        #else
            OwnerRef = new WeakReference<IOwner> (owner)
        #endif
            Ident = ident
            Action = action
        }

type IBus<'evt> =
    abstract AddWatcher : IOwner -> string -> ('evt -> unit) -> unit
    abstract SetWatcher : IOwner -> string -> ('evt -> unit) -> bool    // -> isNew
    abstract RemoveWatcher' : IOwner -> string list                      // -> ident list
    abstract RemoveWatcher' : IOwner * string -> string list             // -> ident list
    abstract RemoveWatcher : IOwner -> unit
    abstract RemoveWatcher : IOwner * string -> unit

[<StructuredFormatDisplay("{AsString}")>]
type Bus<'evt> (owner') =
    let owner : IOwner = owner'
    let mutable logAddRemove = true
    let mutable watchers : Watcher<'evt> list = []
    let setLogAddRemove (v : bool) =
        logAddRemove <- v
    let addWatcher watcher =
        watchers <- watchers @ [ watcher ]
        if logAddRemove then
            owner.Log <| tplWatcherInfo "Bus:Watcher_Added" watcher ()
        else
            owner.Log <| tplWatcherDebug "Bus:Watcher_Added" watcher ()
    let removeWatchers (toRemove : Watcher<'evt> list) =
        watchers <-
            watchers
            |> List.filter (fun w ->
                let check = fun w' ->
                    w.OwnerIdent = w'.OwnerIdent
                    && w.Ident = w'.Ident
                not (List.exists check toRemove)
            )
        if logAddRemove then
            owner.Log <| tplWatcherInfo "Bus:Watchers_Removed" toRemove ()
        else
            owner.Log <| tplWatcherDebug "Bus:Watchers_Removed" toRemove ()
    let tryGetTarget (watcher : Watcher<'evt>) =
    #if FABLE_COMPILER
        (true, watcher.OwnerRef)
    #else
        watcher.OwnerRef.TryGetTarget ()
    #endif
    override _this.ToString () : string =
    #if FABLE_COMPILER
        let prefixFormat = sprintf "[Bus %s <%i>]" owner.Ident
    #else
        let prefixFormat = sprintf "[Bus<%s> %s <%i>]" (typeof<'evt>.FullName) owner.Ident
    #endif
        prefixFormat watchers.Length
    member this.AsString =
        this.ToString ()
    member _this.Trigger (evt : 'evt) =
        if owner.Disposed then
            owner.Log <| tplDisposedInfo "Bus:Owner_Disposed" owner evt
        else
            let folder = fun garbage watcher ->
                match tryGetTarget watcher with
                | (true, owner) ->
                    if owner.Disposed then
                        owner.Log <| tplDisposedInfo "Bus:Watcher_Disposed" owner evt
                        true
                    else
                        try
                            watcher.Action evt
                            owner.Log <| tplWatcherSucceed "Bus:Notify_Succeed" watcher evt
                        with e ->
                            owner.Log <| tplWatcherFailed "Bus:Notify_Failed" watcher evt e
                        false
                | (false, _) ->
                    true
                |> function
                    | true ->
                        match garbage with
                        | None -> Some [watcher]
                        | Some garbage -> Some <| watcher :: garbage
                    | false ->
                        garbage
            watchers
            |> List.fold folder None
            |> function
                | None -> ()
                | Some garbage ->
                    removeWatchers garbage
    member _this.TryFindWatchers (owner : IOwner) (ident : string option) =
        watchers
        |> List.filter (fun watcher ->
            ident
            |> Option.map (fun ident -> ident = watcher.Ident)
            |> Option.defaultValue true
            |> function
            | true ->
                match tryGetTarget watcher with
                | (true, owner') ->
                    owner.Ident = owner'.Ident
                | (false, _) ->
                    false
            | false ->
                false
        )
    member this.Publish : IBus<'evt> =
        { new IBus<'evt> with
            member _x.AddWatcher owner ident action =
                match this.TryFindWatchers owner (Some ident) with
                | [] ->
                    let watcher = Watcher<'evt>.Create owner ident action
                    addWatcher watcher
                | old ->
                    owner.Log <| tplAddWatcherError "Bus:Watcher_Already_Exist" owner ident action old
            member _x.SetWatcher owner ident action =
                let watcher = Watcher<'evt>.Create owner ident action
                match this.TryFindWatchers owner (Some ident) with
                | [] ->
                    addWatcher watcher
                    true
                | old ->
                    removeWatchers old
                    addWatcher watcher
                    false
            member _x.RemoveWatcher' owner =
                match this.TryFindWatchers owner None with
                | [] ->
                    []
                | old ->
                    removeWatchers old
                    old |> List.map (fun w -> w.Ident)
            member _x.RemoveWatcher' (owner, ident) =
                match this.TryFindWatchers owner (Some ident) with
                | [] ->
                    []
                | old ->
                    removeWatchers old
                    old |> List.map (fun w -> w.Ident)
            member x.RemoveWatcher owner =
                x.RemoveWatcher' owner |> ignore
            member x.RemoveWatcher (owner, ident) =
                x.RemoveWatcher' (owner, ident) |> ignore
        }
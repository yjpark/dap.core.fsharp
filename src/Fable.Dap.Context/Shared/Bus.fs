[<AutoOpen>]
module Dap.Context.Bus

open System
open Dap.Prelude

// This is to provide some extra feature to Event<> and IEvent<>
//
// Related Readings:
// - https://www.codeproject.com/Articles/29922/Weak-Events-in-C
// - https://sachabarbs.wordpress.com/2014/04/23/f-21-events/

let private tplBusError = LogEvent.Template2WithException<string, obj>(LogLevelError, "[{Section}] {Msg} -> Failed")

let private tplDisposedInfo = LogEvent.Template3<string, obj, obj>(LogLevelInformation, "[{Section}] {Owner} {Detail}")

let private tplWatchersInfo = LogEvent.Template3<string, obj, obj>(LogLevelInformation, "[{Section}] {Watchers} {Detail}")

let private tplWatchersDebug = LogEvent.Template3<string, obj, obj>(LogLevelDebug, "[{Section}] {Watchers} {Detail}")

let private tplWatcherSucceed = LogEvent.Template3<string, obj, obj>(LogLevelDebug, "[{Section}] {Watcher} {Detail}")

let private tplWatcherInfo = LogEvent.Template3<string, obj, obj>(LogLevelInformation, "[{Section}] {Watcher} {Detail}")

let private tplWatcherDebug = LogEvent.Template3<string, obj, obj>(LogLevelDebug, "[{Section}] {Watcher} {Detail}")

let private tplWatcherFailed = LogEvent.Template3WithException<string, obj, obj>(LogLevelError, "[{Section}] {Watcher} {Detail} -> Failed")

let private tplAddWatcherError = LogEvent.Template5<string, IOwner, Luid, obj, obj>(LogLevelError, "[{Section}] {Owner} {Luid} {Action} -> {Old}")

type Watcher<'msg> = {
    OwnerLuid : Luid
#if FABLE_COMPILER
    OwnerRef : IOwner
#else
    OwnerRef : WeakReference<IOwner>
#endif
    Luid : Luid
    Action : 'msg -> unit
} with
    static member Create (owner : IOwner) luid action =
        {
            OwnerLuid = owner.Luid
        #if FABLE_COMPILER
            OwnerRef = owner
        #else
            OwnerRef = new WeakReference<IOwner> (owner)
        #endif
            Luid = luid
            Action = action
        }

[<StructuredFormatDisplay("{AsString}")>]
type Bus<'msg> (owner') =
    let owner : IOwner = owner'
    let mutable logAddRemove = true
    let mutable watchers : Watcher<'msg> list = []
    let setLogAddRemove (v : bool) =
        logAddRemove <- v
    let addWatcher watcher =
        watchers <- watchers @ [ watcher ]
        if logAddRemove then
            owner.Log <| tplWatcherInfo "Bus:Watcher_Added" watcher ()
        else
            owner.Log <| tplWatcherDebug "Bus:Watcher_Added" watcher ()
    let removeWatchers (toRemove : Watcher<'msg> list) =
        watchers <-
            watchers
            |> List.filter (fun w ->
                let check = fun w' ->
                    w.OwnerLuid = w'.OwnerLuid
                    && w.Luid = w'.Luid
                not (List.exists check toRemove)
            )
        if logAddRemove then
            owner.Log <| tplWatcherInfo "Bus:Watchers_Removed" toRemove ()
        else
            owner.Log <| tplWatcherDebug "Bus:Watchers_Removed" toRemove ()
    let tryGetTarget (watcher : Watcher<'msg>) =
    #if FABLE_COMPILER
        (true, watcher.OwnerRef)
    #else
        watcher.OwnerRef.TryGetTarget ()
    #endif
    override __.ToString () : string =
    #if FABLE_COMPILER
        let prefixFormat = sprintf "[Bus %s <%i>]" owner.Luid
    #else
        let prefixFormat = sprintf "[Bus<%s> %s <%i>]" (typeof<'msg>.FullName) owner.Luid
    #endif
        prefixFormat watchers.Length
    member this.AsString =
        this.ToString ()
    member __.HasWatchers = watchers.Length > 0
    member __.Trigger (evt : 'msg) =
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
    member __.TryFindWatchers (owner : IOwner) (luid : string option) =
        watchers
        |> List.filter (fun watcher ->
            luid
            |> Option.map (fun luid -> luid = watcher.Luid)
            |> Option.defaultValue true
            |> function
            | true ->
                match tryGetTarget watcher with
                | (true, owner') ->
                    owner.Luid = owner'.Luid
                | (false, _) ->
                    false
            | false ->
                false
        )
    member this.Publish : IBus<'msg> =
        { new IBus<'msg> with
            member _x.AddWatcher owner luid action =
                match this.TryFindWatchers owner (Some luid) with
                | [] ->
                    let watcher = Watcher<'msg>.Create owner luid action
                    addWatcher watcher
                | old ->
                    owner.Log <| tplAddWatcherError "Bus:Watcher_Already_Exist" owner luid action old
            member _x.SetWatcher owner luid action =
                let watcher = Watcher<'msg>.Create owner luid action
                match this.TryFindWatchers owner (Some luid) with
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
                    old |> List.map (fun w -> w.Luid)
            member _x.RemoveWatcher' (owner, luid) =
                match this.TryFindWatchers owner (Some luid) with
                | [] ->
                    []
                | old ->
                    removeWatchers old
                    old |> List.map (fun w -> w.Luid)
            member x.RemoveWatcher owner =
                x.RemoveWatcher' owner |> ignore
            member x.RemoveWatcher (owner, luid) =
                x.RemoveWatcher' (owner, luid) |> ignore
        }
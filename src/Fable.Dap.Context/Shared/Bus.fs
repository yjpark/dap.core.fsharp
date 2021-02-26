[<AutoOpen>]
module Dap.Context.Bus

open System
open Dap.Prelude

// This is to provide some extra feature to Event<> and IEvent<>
//
// Related Readings:
// - https://www.codeproject.com/Articles/29922/Weak-Events-in-C
// - https://sachabarbs.wordpress.com/2014/04/23/f-21-events/

let private tplBusError = LogEvent.Template3WithException<string, Luid, obj>(LogLevelError, "[{Section}] {Luid} {Msg} -> Failed")

let private tplDisposedInfo = LogEvent.Template4<string, Luid, obj, obj>(LogLevelInformation, "[{Section}] {Luid} {Owner} {Detail}")

let private tplWatchersInfo = LogEvent.Template4<string, Luid, obj, obj>(LogLevelInformation, "[{Section}] {Luid} {Watchers} {Detail}")

let private tplWatchersDebug = LogEvent.Template4<string, Luid, obj, obj>(LogLevelDebug, "[{Section}] {Luid} {Watchers} {Detail}")

let private tplWatcherSucceed = LogEvent.Template4<string, Luid, obj, obj>(LogLevelDebug, "[{Section}] {Luid} {Watcher} {Detail}")

let private tplWatcherInfo = LogEvent.Template4<string, Luid, obj, obj>(LogLevelInformation, "[{Section}] {Luid} {Watcher} {Detail}")

let private tplWatcherDebug = LogEvent.Template4<string, Luid, obj, obj>(LogLevelDebug, "[{Section}] {Luid} {Watcher} {Detail}")

let private tplWatcherFailed = LogEvent.Template4WithException<string, Luid, obj, obj>(LogLevelError, "[{Section}] {Luid} {Watcher} {Detail} -> Failed")

let private tplAddWatcherError = LogEvent.Template6<string, Luid, IOwner, Luid, obj, obj>(LogLevelError, "[{Section}] {Luid} {WatcherOwner} {WatcherLuid} {Action} -> {Old}")

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
type Bus<'msg> (owner : IOwner, luid : Luid) =
    let mutable ver = 0
    let mutable logAddRemove = true
    let mutable watchers : Watcher<'msg> list = []
    let setLogAddRemove (v : bool) =
        logAddRemove <- v
    let addWatcher watcher =
        watchers <- watchers @ [ watcher ]
        if logAddRemove then
            owner.Log <| tplWatcherInfo "Bus:Watcher_Added" luid watcher ()
        else
            owner.Log <| tplWatcherDebug "Bus:Watcher_Added" luid watcher ()
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
            owner.Log <| tplWatcherInfo "Bus:Watchers_Removed" luid toRemove ()
        else
            owner.Log <| tplWatcherDebug "Bus:Watchers_Removed" luid toRemove ()
    let tryGetTarget (watcher : Watcher<'msg>) =
    #if FABLE_COMPILER
        (true, watcher.OwnerRef)
    #else
        watcher.OwnerRef.TryGetTarget ()
    #endif
    override __.ToString () : string =
    #if FABLE_COMPILER
        let prefixFormat = sprintf "[Bus %s %s %i]" owner.Luid luid
    #else
        let prefixFormat = sprintf "[Bus<%s> %s %s %i]" (typeof<'msg>.FullName) owner.Luid luid
    #endif
        prefixFormat watchers.Length
    member this.AsString =
        this.ToString ()
    member __.HasWatchers = watchers.Length > 0
    member __.Trigger (evt : 'msg) =
        if owner.Disposed then
            owner.Log <| tplDisposedInfo "Bus:Owner_Disposed" luid owner evt
        else
            ver <- ver + 1
            let folder = fun garbage watcher ->
                match tryGetTarget watcher with
                | (true, owner) ->
                    if owner.Disposed then
                        owner.Log <| tplDisposedInfo "Bus:Watcher_Disposed" luid owner evt
                        true
                    else
                        try
                            watcher.Action evt
                            owner.Log <| tplWatcherSucceed "Bus:Notify_Succeed" luid watcher evt
                        with e ->
                            owner.Log <| tplWatcherFailed "Bus:Notify_Failed" luid watcher evt e
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
    member __.TryFindWatchers (watcherOwner : IOwner) (watcherLuid : string option) =
        watchers
        |> List.filter (fun watcher ->
            watcherLuid
            |> Option.map (fun watcherLuid -> watcherLuid = watcher.Luid)
            |> Option.defaultValue true
            |> function
            | true ->
                match tryGetTarget watcher with
                | (true, watcherOwner') ->
                    watcherOwner.Luid = watcherOwner'.Luid
                | (false, _) ->
                    false
            | false ->
                false
        )
    member this.Publish : IBus<'msg> =
        { new IBus<'msg> with
            member _x.Ver = ver
            member _x.AddWatcher watcherOwner watcherLuid action =
                match this.TryFindWatchers watcherOwner (Some watcherLuid) with
                | [] ->
                    let watcher = Watcher<'msg>.Create watcherOwner watcherLuid action
                    addWatcher watcher
                | old ->
                    owner.Log <| tplAddWatcherError "Bus:Watcher_Already_Exist" luid watcherOwner watcherLuid action old
            member _x.SetWatcher watcherOwner watcherLuid action =
                let watcher = Watcher<'msg>.Create watcherOwner watcherLuid action
                match this.TryFindWatchers watcherOwner (Some watcherLuid) with
                | [] ->
                    addWatcher watcher
                    true
                | old ->
                    removeWatchers old
                    addWatcher watcher
                    false
            member _x.RemoveWatcher' watcherOwner =
                match this.TryFindWatchers watcherOwner None with
                | [] ->
                    []
                | old ->
                    removeWatchers old
                    old |> List.map (fun w -> w.Luid)
            member _x.RemoveWatcher1' (watcherOwner, watcherLuid) =
                match this.TryFindWatchers watcherOwner (Some watcherLuid) with
                | [] ->
                    []
                | old ->
                    removeWatchers old
                    old |> List.map (fun w -> w.Luid)
            member x.RemoveWatcher watcherOwner =
                x.RemoveWatcher' watcherOwner |> ignore
            member x.RemoveWatcher1 (watcherOwner, watcherLuid) =
                x.RemoveWatcher1' (watcherOwner, watcherLuid) |> ignore
            member x.Owner = owner
        }
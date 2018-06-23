[<AutoOpen>]
module Dap.Platform.Bus

open System
open Dap.Prelude
open System

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

let private tplWatcherInfo = LogEvent.Template3<string, obj, obj>(LogLevelInformation, "[{Section}] {Watcher} {Detail}")

let private tplWatchersInfo = LogEvent.Template3<string, obj, obj>(LogLevelInformation, "[{Section}] {Watchers} {Detail}")

let private tplWatcherError = LogEvent.Template3<string, obj, obj>(LogLevelError, "[{Section}] {Watcher} {Detail} -> Failed")

let private tplWatcherFailed = LogEvent.Template3WithException<string, obj, obj>(LogLevelError, "[{Section}] {Watcher} {Detail} -> Failed")

let private tplAddWatcherError = LogEvent.Template5<string, IOwner, string, obj, obj>(LogLevelError, "[{Section}] {Owner} {Ident} {Action} -> {Old}")

type Watcher<'evt> = {
    OwnerIdent : string
    OwnerRef : WeakReference<IOwner>
    Ident : string
    Action : 'evt -> unit
} with
    static member Create (owner : IOwner) ident action =
        {
            OwnerIdent = owner.Ident
            OwnerRef = new WeakReference<IOwner> (owner)
            Ident = ident
            Action = action
        }

type IBus<'evt> =
    abstract AddWatcher : IOwner -> string -> ('evt -> unit) -> unit
    abstract SetWatcher : IOwner -> string -> ('evt -> unit) -> bool    // -> isNew
    abstract RemoveWatcher : IOwner -> string list                      // -> ident list
    abstract RemoveWatcher : IOwner * string -> string list             // -> ident list

[<StructuredFormatDisplay("{AsString}")>]
type Bus<'evt> (owner') =
    let owner : IOwner = owner'
    let mutable watchers : Watcher<'evt> list = []
    let removeWatchers (toRemove : Watcher<'evt> list) =
        watchers <- 
            watchers
            |> List.filter (fun w ->
                let check = fun w' ->
                    w.OwnerIdent = w'.OwnerIdent
                    && w.Ident = w'.Ident
                not (List.exists check toRemove)
            )
        owner.Log <| tplWatchersInfo "Bus:Watchers_Removed" toRemove ()
    override _this.ToString () : string =
        let prefixFormat = sprintf "[Bus<%s> %s <%i>]" (typeof<'evt>.FullName) owner.Ident
        prefixFormat watchers.Length
    member this.AsString =
        this.ToString ()
    member _this.Trigger (evt : 'evt) =
        if owner.Disposed then
            owner.Log <| tplDisposedInfo "Bus:Self_Disposed" owner evt
        else
            let folder = fun garbage watcher ->
                match watcher.OwnerRef.TryGetTarget () with
                | (true, owner) ->
                    if owner.Disposed then
                        owner.Log <| tplDisposedInfo "Bus:Watcher_Disposed" owner evt
                        true
                    else
                        try
                            watcher.Action evt
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
                match watcher.OwnerRef.TryGetTarget () with
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
                    let watcher =
                        {
                            OwnerIdent = owner.Ident
                            Ident = ident
                            Action = action
                            OwnerRef = new WeakReference<IOwner> (owner)
                        }
                    watchers <- watchers @ [ watcher ]
                | old ->
                    owner.Log <| tplAddWatcherError "Bus:Watcher_Already_Exist" owner ident action old
            member _x.SetWatcher owner ident action =
                let watcher =
                    {
                        OwnerIdent = owner.Ident
                        Ident = ident
                        Action = action
                        OwnerRef = new WeakReference<IOwner> (owner)
                    }
                match this.TryFindWatchers owner (Some ident) with
                | [] ->
                    watchers <- watchers @ [ watcher ]
                    true
                | old ->
                    removeWatchers old
                    watchers <- watchers @ [ watcher ]
                    false
            member _x.RemoveWatcher (owner, ident) =
                match this.TryFindWatchers owner (Some ident) with
                | [] ->
                    []
                | old ->
                    removeWatchers old
                    old |> List.map (fun w -> w.Ident)
            member _x.RemoveWatcher owner =
                match this.TryFindWatchers owner None with
                | [] ->
                    []
                | old ->
                    removeWatchers old
                    old |> List.map (fun w -> w.Ident)
        }
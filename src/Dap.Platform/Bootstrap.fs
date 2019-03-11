[<AutoOpen>]
[<RequireQualifiedAccess>]
module Dap.Platform.Bootstrap

open System
open System.Reflection

open Dap.Prelude
open Dap.Context
open Dap.Context.Unsafe

let private typeIObj = typeof<IObj>
let private typeILogger = typeof<ILogger>
let private typeIOwner = typeof<IOwner>
let private typeIJson = typeof<IJson>
let private typeIContext = typeof<IContext>
let private typeIUnsafeContext = typeof<IUnsafeContext>
let private typeIFeature = typeof<IFeature>
let private typeIFallback = typeof<IFallback>
let private typeIOverride = typeof<IOverride>
let private typeIHook = typeof<IHook>

let private isHook (type' : Type) =
    Array.contains typeIHook <| type'.GetInterfaces ()

let private isFeature (type' : Type) =
    Array.contains typeIFeature <| type'.GetInterfaces ()

let private isOverride (type' : Type) =
    Array.contains typeIOverride <| type'.GetInterfaces ()

let private isFallback (type' : Type) =
    if isOverride type' then
        false
    else
        Array.contains typeIFallback <| type'.GetInterfaces ()

let mutable private bootstrapped : bool = false
let mutable private hooks : Map<string, Type list> = Map.empty
let mutable private features : Map<string, Type> = Map.empty

let getKind (type' : Type) = type'.FullName

let private getFeatureKinds (type' : Type) =
    type'.GetInterfaces ()
    |> Array.filter (fun t ->
        (t.GetGenericArguments ()) .Length = 0
            && t <> typeIObj
            && t <> typeILogger
            && t <> typeIOwner
            && t <> typeIJson
            && t <> typeIContext
            && t <> typeIUnsafeContext
            && t <> typeIFeature
            && t <> typeIFallback
            && t <> typeIOverride
    )|> Array.map getKind

let private getHookKinds (type' : Type) =
    type'.GetInterfaces ()
    |> Array.filter (fun t ->
        (t.GetGenericArguments ()) .Length = 0
            && t <> typeIObj
            && t <> typeILogger
            && t <> typeIOwner
            && t <> typeIJson
            && t <> typeIContext
            && t <> typeIUnsafeContext
            && t <> typeIHook
    )|> Array.map getKind

let private addHook (logger : ILogger) ((kind, type') : string * Type) : unit =
    match Map.tryFind kind hooks with
    | None ->
        hooks <-
            Map.add kind [ type' ] hooks
    | Some types ->
        hooks <-
            Map.add kind (type' :: types) hooks

let private addFeature (logger : ILogger) ((kind, type') : string * Type) : unit =
    match Map.tryFind kind features with
    | None ->
        features <-
            Map.add kind type' features
    | Some oldType ->
        if isOverride type'
            || isFallback oldType then
            logInfo logger "Feature_Overridden" kind (oldType, "->", type')
            features <-
                Map.add kind type' features
        elif isOverride oldType
            || isFallback type' then
            logInfo logger "Feature_Overridden" kind (type', "->", oldType)
        else
            logError logger "Feature_Conflicted" kind (oldType, type')

let private tryLoadTypes (logger : ILogger) (assembly : Assembly) =
    try
        assembly.GetTypes ()
    with e ->
        logWarn logger "LoadTypes_Failed" assembly.FullName (e)
        [| |]

let logHooks (logger : ILogger) =
    logWarn logger "Hooks" "Total" (sprintf "[%d]" hooks.Count)
    hooks
    |> Map.iter (fun kind types ->
        let tip = (sprintf "<%s>" kind)
        logWarn logger "Hooks" tip (sprintf "[%d]" types.Length)
        types
        |> List.iter (fun type' ->
            logWarn logger "Hooks" tip type'.FullName
        )
    )

let logFeatures (logger : ILogger) =
    logWarn logger "Features" "Total" (sprintf "[%d]" features.Count)
    features
    |> Map.iter (fun kind type' ->
        logWarn logger "Features" (sprintf "<%s>" kind) type'.FullName
    )

let private bootstrap (logging : ILogging) =
    let logger = logging.GetLogger "Bootstrap"
    AppDomain.CurrentDomain.GetAssemblies ()
    |> Array.iter (fun assembly ->
        logInfo logger "Assembly" assembly.FullName (assembly.CodeBase)
        tryLoadTypes logger assembly
        |> Array.iter (fun t ->
            if not t.IsInterface && not t.IsAbstract then
                if isHook t then
                    let hookKinds = getHookKinds t
                    logInfo logger "Hook" t.FullName hookKinds
                    hookKinds
                    |> Array.iter (fun kind ->
                        addHook logger (kind, t)
                    )
                if isFeature t then
                    let featureKinds = getFeatureKinds t
                    logInfo logger "Feature" t.FullName featureKinds
                    featureKinds
                    |> Array.iter (fun kind ->
                        addFeature logger (kind, t)
                    )
        )
    )
    logHooks logger
    logFeatures logger
    bootstrapped <- true

let private bootstrapLock = obj ()

let getHooks (logging : ILogging) =
    let exec = fun () ->
        if not bootstrapped then
            bootstrap logging
        hooks
    lock bootstrapLock exec

let getFeatures (logging : ILogging) =
    let exec = fun () ->
        if not bootstrapped then
            bootstrap logging
        features
    lock bootstrapLock exec

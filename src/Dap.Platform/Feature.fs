[<AutoOpen>]
module Dap.Platform.Feature

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

let private isFeature (type' : Type) =
    Array.contains typeIFeature <| type'.GetInterfaces ()

let private isOverride (type' : Type) =
    Array.contains typeIOverride <| type'.GetInterfaces ()

let private isFallback (type' : Type) =
    if isOverride type' then
        false
    else
        Array.contains typeIFallback <| type'.GetInterfaces ()

let mutable private features : Map<string, Type> option = None

let private getKind (type' : Type) = type'.FullName

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

let private addFeature (logger : ILogger) (features : Map<string, Type>) ((kind, type') : string * Type) : Map<string, Type> =
    match Map.tryFind kind features with
    | None ->
        Map.add kind type' features
    | Some oldType ->
        if isOverride type'
            || isFallback oldType then
            logInfo logger "Override" kind (oldType, type')
            Map.add kind type' features
        elif isOverride oldType
            || isFallback type' then
            logInfo logger "Override" kind (type', oldType)
            features
        else
            logError logger "Conflicted" kind (oldType, type')
            Map.add kind type' features

let private tryLoadTypes (logger : ILogger) (assembly : Assembly) =
    try
        assembly.GetTypes ()
    with e ->
        logWarn logger "LoadTypes_Failed" assembly.FullName (e)
        [| |]

let logFeatures (logger : ILogger) (features : Map<string, Type>) =
    logWarn logger "Total_Loaded" (sprintf "[%d]" features.Count) ()
    features
    |> Map.iter (fun kind type' ->
        logWarn logger "Loaded" (sprintf "<%s>" kind) type'.FullName
    )

let private loadFeatures (logging : ILogging) : Map<string, Type> =
    let logger = logging.GetLogger "LoadFeatures"
    AppDomain.CurrentDomain.GetAssemblies ()
    |> Array.map (fun assembly ->
        logInfo logger "Assembly" assembly.FullName (assembly.CodeBase)
        tryLoadTypes logger assembly
        |> Array.filter (fun t ->
            isFeature t
                && not t.IsInterface
                && not t.IsAbstract
        )|> Array.map (fun type' ->
            logInfo logger "Type" type'.FullName (getFeatureKinds type')
            getFeatureKinds type'
            |> Array.map (fun kind ->
                (kind, type')
            )
        )|> Array.concat
    )|> Array.concat
    |> Array.fold (addFeature logger) Map.empty
    |> fun features -> logFeatures logger features ; features

let private getFeaturesLock = obj ()

let getFeatures (logging : ILogging) =
    let exec = fun () ->
        if features.IsNone then
            features <- Some <| loadFeatures logging
        features.Value
    lock getFeaturesLock exec


let create<'feature when 'feature :> IFeature> (logging : ILogging) : 'feature =
    let kind = getKind typeof<'feature>
    let notLoaded = features.IsNone
    getFeatures logging
    |> Map.tryFind kind
    |> function
        | Some type' ->
            Activator.CreateInstance (type', [| (logging :> obj) |])
            :?> 'feature
        | None ->
            failWith "Feature_Not_Found" kind

let addToAgent<'feature when 'feature :> IFeature> (agent : IAgent) : 'feature =
    create<'feature> agent.Env.Logging

let createLogging (args : LoggingArgs) : ILogging =
    let provider = create<ILoggingProvider> (getLogging ())
    provider.CreateLogging args

let tryStartApp<'app when 'app :> IBaseApp> (app : 'app) : unit =
    if app.SetupResult.IsNone then
        let runner = create<IAppRunner> (app.Env.Logging)
        runner.Start app

let startApp<'app when 'app :> IBaseApp> (app : 'app) : unit =
    if app.SetupResult.IsNone then
        tryStartApp<'app> app
    else
        failWith "Already_Setup" app.SetupResult.Value

type IApp<'app when 'app :> IBaseApp> with
    member this.SetupStarted =
        this.SetupResult.IsSome
    member this.SetupFinished =
        this.SetupResult.IsSome
            && (this.SetupResult.Value.IsError || this.SetupResult.Value.Value)
    member this.SetupSucceed =
        this.SetupResult.IsSome && this.SetupResult.Value.IsOk && this.SetupResult.Value.Value
    member this.SetupFailed =
        this.SetupResult.IsSome && this.SetupResult.Value.IsError
    member this.SetupError =
        this.SetupResult.Value.ErrorValue
    member this.TryStart () =
        tryStartApp this.Runner
    member this.Start () =
        startApp this.Runner
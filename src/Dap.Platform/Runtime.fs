[<AutoOpen>]
[<RequireQualifiedAccess>]
module Dap.Platform.Runtime

open System
open System.Reflection
open System.Runtime.InteropServices

open Dap.Prelude
open Dap.Context
open Dap.Platform

[<RequireQualifiedAccess>]
type NetCorePlatform =
    | Mac
    | Linux
    | Windows

[<RequireQualifiedAccess>]
type XamarinPlatform =
    | IOS
    | Android
    | Other of string

[<RequireQualifiedAccess>]
type WindowsPlatform =
    | UWP
    | WPF

[<RequireQualifiedAccess>]
type Platform =
    | NetCore of NetCorePlatform
    | Xamarin of XamarinPlatform
    | Windows of WindowsPlatform
    | Unknown of string

type IRuntime =
    abstract Platform : Platform with get
    abstract DataFolder : string with get
    abstract CacheFolder : string with get
    abstract CreateLogging : LoggingArgs -> ILogging

type NetCoreRuntime () =
    interface IRuntime with
        member __.Platform =
            if RuntimeInformation.IsOSPlatform (OSPlatform.OSX) then
                Platform.NetCore NetCorePlatform.Mac
            elif RuntimeInformation.IsOSPlatform (OSPlatform.Linux) then
                Platform.NetCore NetCorePlatform.Linux
            elif RuntimeInformation.IsOSPlatform (OSPlatform.Windows) then
                Platform.NetCore NetCorePlatform.Windows
            else
                Platform.Unknown RuntimeInformation.OSDescription
        member __.DataFolder = ""
        member __.CacheFolder = ""
        member __.CreateLogging (args : LoggingArgs) =
            let newArgs = args.WithFolder("log")
            let logging = newArgs.ToSerilogLogging ()
            logInfo logging "NetCoreRuntime" "CreateLogging" (encodeJson 4 newArgs)
            if newArgs.File.IsSome then
                let note = sprintf "%s -> %s", args.File.Value.Folder, newArgs.File.Value.Folder
                logInfo logging "NetCoreRuntime" "Folder_Updated" note
            logging :> ILogging
    interface IFallback

let private typeIRuntime = typeof<IRuntime>
let private typeIFallback = typeof<IFallback>

let private isRuntime (type' : Type) =
    Array.contains typeIRuntime <| type'.GetInterfaces ()

let private isFallback (type' : Type) =
    Array.contains typeIFallback <| type'.GetInterfaces ()

let private tryLoadTypes (logger : ILogger) (assembly : Assembly) =
    try
        assembly.GetTypes ()
    with e ->
        logWarn logger "LoadTypes_Failed" assembly.FullName (e)
        [| |]

let private pickRuntime (logger : ILogger) (current : Type option) (type' : Type) : Type option =
    match current with
    | None ->
        Some type'
    | Some oldType ->
        if isFallback oldType then
            Some type'
        elif isFallback type' then
            Some oldType
        else
            logError logger "Conflicted" type'.FullName (oldType, type')
            Some type'

let loadRuntime () =
    let logger = new FallbackLogger ("LoadRuntime")
    AppDomain.CurrentDomain.GetAssemblies ()
    |> Array.map (fun assembly ->
        logInfo logger "Assembly" assembly.FullName (assembly.CodeBase)
        tryLoadTypes logger assembly
        |> Array.filter (fun t ->
            isRuntime t
                && not t.IsInterface
                && not t.IsAbstract
        )
    )|> Array.concat
    |> Array.fold (pickRuntime logger) None
    |> fun type' ->
        Activator.CreateInstance (type'.Value, [| |])
        :?> IRuntime
let private runtime = loadRuntime ()

type Runtime = Runtime with
    static member Instance = runtime
    static member Platform = runtime.Platform
    static member CreateLogging = runtime.CreateLogging
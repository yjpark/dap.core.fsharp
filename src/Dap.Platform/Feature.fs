[<AutoOpen>]
[<RequireQualifiedAccess>]
module Dap.Platform.Feature

open System
open System.Threading.Tasks

open Dap.Prelude
open Dap.Context
open Dap.Context.Unsafe

let tryCreate<'feature when 'feature :> IFeature> (logging : ILogging) : 'feature option =
    let kind = Bootstrap.getKind typeof<'feature>
    Bootstrap.getFeatures logging
    |> Map.tryFind kind
    |> Option.bind (fun type' ->
        try
            Activator.CreateInstance (type', [| (logging :> obj) |])
            :?> 'feature
            |> Some
        with e ->
            logException logging "Feature.tryCreate" "Exception_Raised" (typeof<'feature>, type') e
            None
    )

let create<'feature when 'feature :> IFeature> (logging : ILogging) : 'feature =
    tryCreate<'feature> logging
    |> Option.defaultWith (fun () ->
        let kind = getKind typeof<'feature>
        failWith "Feature_Not_Created" kind
    )

let addToAgent<'feature when 'feature :> IFeature> (agent : IAgent) : 'feature =
    create<'feature> agent.Env.Logging

let createLogging (args : LoggingArgs) : ILogging =
    let provider = create<ILoggingProvider> (getLogging ())
    provider.CreateLogging args
    |> (fun logging ->
        let logger = logging.GetLogger "Bootstrap"
        logFeatures logger
        logHooks logging
        logCliHooks logging
        logging
    )

let tryStartApp<'app when 'app :> IBaseApp> (app : 'app) : unit =
    if app.SetupResult.IsNone then
        let runner = create<IAppRunner> (app.Env.Logging)
        runner.Start app

let startApp<'app when 'app :> IBaseApp> (app : 'app) : unit =
    if app.SetupResult.IsNone then
        tryStartApp<'app> app
    else
        failWith "Already_Setup" app.SetupResult.Value

let tryStartAppAsync<'app when 'app :> IBaseApp> (app : 'app) : Task<unit> = task {
    if app.SetupResult.IsNone then
        let runner = create<IAppRunner> (app.Env.Logging)
        do! runner.StartAsync app
}

let startAppAsync<'app when 'app :> IBaseApp> (app : 'app) : Task<unit> =
    if app.SetupResult.IsNone then
        tryStartAppAsync<'app> app
    else
        failWith "Already_Setup" app.SetupResult.Value

type IBaseApp with
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
        tryStartApp this
    member this.Start () =
        startApp this
    member this.TryStartAsync () =
        tryStartAppAsync this
    member this.StartAsync () =
        startAppAsync this

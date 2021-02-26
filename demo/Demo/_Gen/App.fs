[<AutoOpen>]
module Demo.App

open System.Threading
open System.Threading.Tasks
open FSharp.Control.Tasks.V2
open Dap.Prelude
open Dap.Context
open Dap.Context.Builder
open Dap.Platform

module TickerTypes = Dap.Platform.Ticker.Types

(*
 * Generated: <Pack>
 *)
type IServicesPackArgs =
    abstract Ticker : TickerTypes.Args with get

type IServicesPack =
    inherit IPack
    abstract Args : IServicesPackArgs with get
    abstract Ticker : TickerTypes.Agent with get

(*
 * Generated: <Pack>
 *)
type ICommonPackArgs =
    inherit IServicesPackArgs
    abstract Common : int with get
    abstract AsServicesPackArgs : IServicesPackArgs with get

type ICommonPack =
    inherit IPack
    inherit IServicesPack
    abstract Args : ICommonPackArgs with get
    abstract AsServicesPack : IServicesPack with get

(*
 * Generated: <Pack>
 *)
type IAppPackArgs =
    inherit ICommonPackArgs
    inherit IServicesPackArgs
    abstract Test : int with get
    abstract AsCommonPackArgs : ICommonPackArgs with get
    abstract AsServicesPackArgs : IServicesPackArgs with get

type IAppPack =
    inherit IPack
    inherit ICommonPack
    inherit IServicesPack
    abstract Args : IAppPackArgs with get
    abstract AsCommonPack : ICommonPack with get
    abstract AsServicesPack : IServicesPack with get

(*
 * Generated: <Pack>
 *)
type IBackupPackArgs =
    inherit ICommonPackArgs
    abstract Backup : TickerTypes.Args with get
    abstract AsCommonPackArgs : ICommonPackArgs with get

type IBackupPack =
    inherit IPack
    inherit ICommonPack
    abstract Args : IBackupPackArgs with get
    abstract Backup : TickerTypes.Agent with get
    abstract AsCommonPack : ICommonPack with get

(*
 * Generated: <App>
 *)

type AppKinds () =
    static member Ticker (* IServicesPack *) = "Ticker"
    static member Backup (* IBackupPack *) = "Backup"

type AppKeys () =
    static member Ticker (* IServicesPack *) = ""
    static member Backup (* IBackupPack *) = ""

type IApp =
    inherit IBaseApp
    inherit IRunner<IApp>
    inherit IAppPack
    inherit IBackupPack
    abstract Args : AppArgs with get
    abstract AsAppPack : IAppPack with get
    abstract AsBackupPack : IBackupPack with get

(*
 * Generated: <Record>
 *     IsJson, IsLoose
 *)
and AppArgs = {
    Scope : (* AppArgs *) Scope
    Setup : (* AppArgs *) IApp -> unit
    Ticker : (* IServicesPack *) TickerTypes.Args
    Common : (* ICommonPack *) int
    Test : (* IAppPack *) int
    Backup : (* IBackupPack *) TickerTypes.Args
} with
    static member Create
        (
            ?scope : (* AppArgs *) Scope,
            ?setup : (* AppArgs *) IApp -> unit,
            ?ticker : (* IServicesPack *) TickerTypes.Args,
            ?common : (* ICommonPack *) int,
            ?test : (* IAppPack *) int,
            ?backup : (* IBackupPack *) TickerTypes.Args
        ) : AppArgs =
        {
            Scope = (* AppArgs *) scope
                |> Option.defaultWith (fun () -> NoScope)
            Setup = (* AppArgs *) setup
                |> Option.defaultWith (fun () -> ignore)
            Ticker = (* IServicesPack *) ticker
                |> Option.defaultWith (fun () -> (TickerTypes.Args.Create ()))
            Common = (* ICommonPack *) common
                |> Option.defaultWith (fun () -> 100)
            Test = (* IAppPack *) test
                |> Option.defaultWith (fun () -> 100)
            Backup = (* IBackupPack *) backup
                |> Option.defaultWith (fun () -> (decodeJsonValue TickerTypes.Args.JsonDecoder """{"frame_rate":1.0,"auto_start":true}"""))
        }
    static member SetScope ((* AppArgs *) scope : Scope) (this : AppArgs) =
        {this with Scope = scope}
    static member SetSetup ((* AppArgs *) setup : IApp -> unit) (this : AppArgs) =
        {this with Setup = setup}
    static member SetTicker ((* IServicesPack *) ticker : TickerTypes.Args) (this : AppArgs) =
        {this with Ticker = ticker}
    static member SetCommon ((* ICommonPack *) common : int) (this : AppArgs) =
        {this with Common = common}
    static member SetTest ((* IAppPack *) test : int) (this : AppArgs) =
        {this with Test = test}
    static member SetBackup ((* IBackupPack *) backup : TickerTypes.Args) (this : AppArgs) =
        {this with Backup = backup}
    static member JsonEncoder : JsonEncoder<AppArgs> =
        fun (this : AppArgs) ->
            E.object [
                yield "scope", Scope.JsonEncoder (* AppArgs *) this.Scope
                yield "ticker", TickerTypes.Args.JsonEncoder (* IServicesPack *) this.Ticker
            ]
    static member JsonDecoder : JsonDecoder<AppArgs> =
        D.object (fun get ->
            {
                Scope = get.Optional.Field (* AppArgs *) "scope" Scope.JsonDecoder
                    |> Option.defaultValue NoScope
                Setup = (* (* AppArgs *)  *) ignore
                Ticker = get.Optional.Field (* IServicesPack *) "ticker" TickerTypes.Args.JsonDecoder
                    |> Option.defaultValue (TickerTypes.Args.Create ())
                Common = (* (* ICommonPack *)  *) 100
                Test = (* (* IAppPack *)  *) 100
                Backup = (* (* IBackupPack *)  *) (decodeJsonValue TickerTypes.Args.JsonDecoder """{"frame_rate":1.0,"auto_start":true}""")
            }
        )
    static member JsonSpec =
        FieldSpec.Create<AppArgs> (AppArgs.JsonEncoder, AppArgs.JsonDecoder)
    interface IJson with
        member this.ToJson () = AppArgs.JsonEncoder this
    interface IObj
    member this.WithScope ((* AppArgs *) scope : Scope) =
        this |> AppArgs.SetScope scope
    member this.WithSetup ((* AppArgs *) setup : IApp -> unit) =
        this |> AppArgs.SetSetup setup
    member this.WithTicker ((* IServicesPack *) ticker : TickerTypes.Args) =
        this |> AppArgs.SetTicker ticker
    member this.WithCommon ((* ICommonPack *) common : int) =
        this |> AppArgs.SetCommon common
    member this.WithTest ((* IAppPack *) test : int) =
        this |> AppArgs.SetTest test
    member this.WithBackup ((* IBackupPack *) backup : TickerTypes.Args) =
        this |> AppArgs.SetBackup backup
    interface IServicesPackArgs with
        member this.Ticker (* IServicesPack *) : TickerTypes.Args = this.Ticker
    member this.AsServicesPackArgs = this :> IServicesPackArgs
    interface ICommonPackArgs with
        member this.Common (* ICommonPack *) : int = this.Common
        member this.AsServicesPackArgs = this.AsServicesPackArgs
    member this.AsCommonPackArgs = this :> ICommonPackArgs
    interface IAppPackArgs with
        member this.Test (* IAppPack *) : int = this.Test
        member this.AsCommonPackArgs = this.AsCommonPackArgs
        member this.AsServicesPackArgs = this.AsServicesPackArgs
    member this.AsAppPackArgs = this :> IAppPackArgs
    interface IBackupPackArgs with
        member this.Backup (* IBackupPack *) : TickerTypes.Args = this.Backup
        member this.AsCommonPackArgs = this.AsCommonPackArgs
    member this.AsBackupPackArgs = this :> IBackupPackArgs

(*
 * Generated: <ValueBuilder>
 *)
type AppArgsBuilder () =
    inherit ObjBuilder<AppArgs> ()
    override __.Zero () = AppArgs.Create ()
    [<CustomOperation("scope")>]
    member __.Scope (target : AppArgs, (* AppArgs *) scope : Scope) =
        target.WithScope scope
    [<CustomOperation("Setup")>]
    member __.Setup (target : AppArgs, (* AppArgs *) setup : IApp -> unit) =
        target.WithSetup setup
    [<CustomOperation("ticker")>]
    member __.Ticker (target : AppArgs, (* IServicesPack *) ticker : TickerTypes.Args) =
        target.WithTicker ticker
    [<CustomOperation("common")>]
    member __.Common (target : AppArgs, (* ICommonPack *) common : int) =
        target.WithCommon common
    [<CustomOperation("test")>]
    member __.Test (target : AppArgs, (* IAppPack *) test : int) =
        target.WithTest test
    [<CustomOperation("backup")>]
    member __.Backup (target : AppArgs, (* IBackupPack *) backup : TickerTypes.Args) =
        target.WithBackup backup

let app_args = new AppArgsBuilder ()

(*
 * Generated: <App>
 *)
type App (param : EnvParam, args : AppArgs) =
    let env = Env.create param
    let mutable setupResult : Result<bool, exn> option = None
    let onSetup = new Bus<Result<bool, exn>> (env, "App.OnSetup")
    let mutable (* IServicesPack *) ticker : TickerTypes.Agent option = None
    let mutable (* IBackupPack *) backup : TickerTypes.Agent option = None
    new (logging : ILogging, a : AppArgs) =
        let platform = Feature.create<IPlatform> logging
        let clock = new RealClock ()
        App (Env.param platform logging a.Scope clock, a)
    new (loggingArgs : LoggingArgs, a : AppArgs) =
        App (Feature.createLogging loggingArgs, a)
    new (a : AppArgs) =
        App (getLogging (), a)
    member this.SetupAsync () : Task<unit> = task {
        if setupResult.IsSome then
            failWith "Already_Setup" setupResult.Value
        try
            setupResult <- Some (Ok false)
            let! (* IServicesPack *) ticker' = env |> Env.addServiceAsync (Dap.Platform.Ticker.Logic.spec args.Ticker) AppKinds.Ticker AppKeys.Ticker
            ticker <- Some ticker'
            let! (* IBackupPack *) backup' = env |> Env.addServiceAsync (Dap.Platform.Ticker.Logic.spec args.Backup) AppKinds.Backup AppKeys.Backup
            backup <- Some backup'
            do! this.SetupAsync' ()
            logInfo env "App.setupAsync" "Setup_Succeed" (encodeJson 4 args)
            args.Setup this.AsApp
            setupResult <- Some (Ok true)
            onSetup.Trigger setupResult.Value
        with e ->
            setupResult <- Some (Error e)
            logException env "App.setupAsync" "Setup_Failed" (encodeJson 4 args) e
            onSetup.Trigger setupResult.Value
            raise e
    }
    abstract member SetupAsync' : unit -> Task<unit>
    default __.SetupAsync' () = task {
        return ()
    }
    member __.Args : AppArgs = args
    member __.Env : IEnv = env
    member __.SetupResult : Result<bool, exn> option = setupResult
    member __.OnSetup : IBus<Result<bool, exn>> = onSetup.Publish
    interface IBaseApp
    interface INeedSetupAsync with
        member this.SetupResult = this.SetupResult
        member this.SetupAsync () = this.SetupAsync ()
        member this.OnSetup = this.OnSetup
    interface IRunner<IApp> with
        member this.Runner = this.AsApp
        member this.RunFunc func = runFunc' this func
        member this.AddTask onFailed getTask = addTask' this onFailed getTask
        member this.RunTask onFailed getTask = runTask' this onFailed getTask
    interface IRunner with
        member __.Clock = env.Clock
        member __.Dash0 = env.Dash0
        member this.RunFunc0 func = runFunc' this func
        member this.AddTask0 onFailed getTask = addTask' this onFailed getTask
        member this.RunTask0 onFailed getTask = runTask' this onFailed getTask
    interface ITaskManager with
        member __.StartTask task = env.StartTask task
        member __.ScheduleTask task = env.ScheduleTask task
        member __.PendingTasksCount = env.PendingTasksCount
        member __.StartPendingTasks () = env.StartPendingTasks ()
        member __.ClearPendingTasks () = env.ClearPendingTasks ()
        member __.RunningTasksCount = env.RunningTasksCount
        member __.CancelRunningTasks () = env.CancelRunningTasks ()
    interface IPack with
        member __.Env : IEnv = env
    interface ILogger with
        member __.Log m = env.Log m
    interface IServicesPack with
        member this.Args = this.Args.AsServicesPackArgs
        member __.Ticker (* IServicesPack *) : TickerTypes.Agent = ticker |> Option.get
    member this.AsServicesPack = this :> IServicesPack
    interface ICommonPack with
        member this.Args = this.Args.AsCommonPackArgs
        member this.AsServicesPack = this.AsServicesPack
    member this.AsCommonPack = this :> ICommonPack
    interface IAppPack with
        member this.Args = this.Args.AsAppPackArgs
        member this.AsCommonPack = this.AsCommonPack
        member this.AsServicesPack = this.AsServicesPack
    member this.AsAppPack = this :> IAppPack
    interface IBackupPack with
        member this.Args = this.Args.AsBackupPackArgs
        member __.Backup (* IBackupPack *) : TickerTypes.Agent = backup |> Option.get
        member this.AsCommonPack = this.AsCommonPack
    member this.AsBackupPack = this :> IBackupPack
    interface IApp with
        member this.Args : AppArgs = this.Args
        member this.AsAppPack = this.AsAppPack
        member this.AsBackupPack = this.AsBackupPack
    member this.AsApp = this :> IApp
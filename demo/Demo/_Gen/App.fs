[<AutoOpen>]
module Demo.App

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
    abstract BackupTicker : TickerTypes.Args with get
    abstract AsCommonPackArgs : ICommonPackArgs with get

type IBackupPack =
    inherit IPack
    inherit ICommonPack
    abstract Args : IBackupPackArgs with get
    abstract BackupTicker : TickerTypes.Agent with get
    abstract AsCommonPack : ICommonPack with get

(*
 * Generated: <App>
 *)

type AppKinds () =
    static member Ticker (* IServicesPack *) = "Ticker"
    static member BackupTicker (* IBackupPack *) = "Ticker"

type AppKeys () =
    static member Ticker (* IServicesPack *) = ""
    static member BackupTicker (* IBackupPack *) = "Backup"

type IApp =
    inherit IPack
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
    BackupTicker : (* IBackupPack *) TickerTypes.Args
} with
    static member Create
        (
            ?scope : Scope,
            ?setup : IApp -> unit,
            ?ticker : TickerTypes.Args,
            ?common : int,
            ?test : int,
            ?backupTicker : TickerTypes.Args
        ) : AppArgs =
        {
            Scope = (* AppArgs *) scope
                |> Option.defaultWith (fun () -> NoScope)
            Setup = (* AppArgs *) setup
                |> Option.defaultWith (fun () -> ignore)
            Ticker = (* IServicesPack *) ticker
                |> Option.defaultWith (fun () -> (TickerTypes.Args.Default ()))
            Common = (* ICommonPack *) common
                |> Option.defaultWith (fun () -> 100)
            Test = (* IAppPack *) test
                |> Option.defaultWith (fun () -> 100)
            BackupTicker = (* IBackupPack *) backupTicker
                |> Option.defaultWith (fun () -> (decodeJsonValue TickerTypes.Args.JsonDecoder """{"frame_rate":1.0,"auto_start":true}"""))
        }
    static member Default () =
        AppArgs.Create (
            NoScope, (* AppArgs *) (* scope *)
            ignore, (* AppArgs *) (* setup *)
            (TickerTypes.Args.Default ()), (* IServicesPack *) (* ticker *)
            100, (* ICommonPack *) (* common *)
            100, (* IAppPack *) (* test *)
            (decodeJsonValue TickerTypes.Args.JsonDecoder """{"frame_rate":1.0,"auto_start":true}""") (* IBackupPack *) (* backupTicker *)
        )
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
    static member SetBackupTicker ((* IBackupPack *) backupTicker : TickerTypes.Args) (this : AppArgs) =
        {this with BackupTicker = backupTicker}
    static member JsonEncoder : JsonEncoder<AppArgs> =
        fun (this : AppArgs) ->
            E.object [
                "scope", Scope.JsonEncoder (* AppArgs *) this.Scope
                "ticker", TickerTypes.Args.JsonEncoder (* IServicesPack *) this.Ticker
            ]
    static member JsonDecoder : JsonDecoder<AppArgs> =
        D.object (fun get ->
            {
                Scope = get.Optional.Field (* AppArgs *) "scope" Scope.JsonDecoder
                    |> Option.defaultValue NoScope
                Setup = (* (* AppArgs *)  *) ignore
                Ticker = get.Optional.Field (* IServicesPack *) "ticker" TickerTypes.Args.JsonDecoder
                    |> Option.defaultValue (TickerTypes.Args.Default ())
                Common = (* (* ICommonPack *)  *) 100
                Test = (* (* IAppPack *)  *) 100
                BackupTicker = (* (* IBackupPack *)  *) (decodeJsonValue TickerTypes.Args.JsonDecoder """{"frame_rate":1.0,"auto_start":true}""")
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
    member this.WithBackupTicker ((* IBackupPack *) backupTicker : TickerTypes.Args) =
        this |> AppArgs.SetBackupTicker backupTicker
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
        member this.BackupTicker (* IBackupPack *) : TickerTypes.Args = this.BackupTicker
        member this.AsCommonPackArgs = this.AsCommonPackArgs
    member this.AsBackupPackArgs = this :> IBackupPackArgs

(*
 * Generated: <ValueBuilder>
 *)
type AppArgsBuilder () =
    inherit ObjBuilder<AppArgs> ()
    override __.Zero () = AppArgs.Default ()
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
    [<CustomOperation("backup_ticker")>]
    member __.BackupTicker (target : AppArgs, (* IBackupPack *) backupTicker : TickerTypes.Args) =
        target.WithBackupTicker backupTicker

let app_args = AppArgsBuilder ()

(*
 * Generated: <App>
 *)
type App (logging : ILogging, args : AppArgs) as this =
    let env = Env.live MailboxPlatform logging args.Scope
    let mutable setupError : exn option = None
    let mutable (* IServicesPack *) ticker : TickerTypes.Agent option = None
    let mutable (* IBackupPack *) backupTicker : TickerTypes.Agent option = None
    let setupAsync (_runner : IRunner) : Task<unit> = task {
        try
            let! (* IServicesPack *) ticker' = env |> Env.addServiceAsync (Dap.Platform.Ticker.Logic.spec args.Ticker) AppKinds.Ticker AppKeys.Ticker
            ticker <- Some ticker'
            let! (* IBackupPack *) backupTicker' = env |> Env.addServiceAsync (Dap.Platform.Ticker.Logic.spec args.BackupTicker) AppKinds.BackupTicker AppKeys.BackupTicker
            backupTicker <- Some backupTicker'
            do! this.SetupAsync' ()
            logInfo env "App.setupAsync" "Setup_Succeed" (encodeJson 4 args)
            args.Setup this.AsApp
        with e ->
            setupError <- Some e
            logException env "App.setupAsync" "Setup_Failed" (encodeJson 4 args) e
            raise e
    }
    do (
        env.RunTask0 raiseOnFailed setupAsync
    )
    new (loggingArgs : LoggingArgs, a : AppArgs) = new App (loggingArgs.CreateLogging (), a)
    new (a : AppArgs) = new App (getLogging (), a)
    abstract member SetupAsync' : unit -> Task<unit>
    default __.SetupAsync' () = task {
        return ()
    }
    member __.Args : AppArgs = args
    member __.Env : IEnv = env
    member __.SetupError : exn option = setupError
    interface ILogger with
        member __.Log m = env.Log m
    interface IPack with
        member __.Env : IEnv = env
    interface IServicesPack with
        member __.Args = this.Args.AsServicesPackArgs
        member __.Ticker (* IServicesPack *) : TickerTypes.Agent = ticker |> Option.get
    member __.AsServicesPack = this :> IServicesPack
    interface ICommonPack with
        member __.Args = this.Args.AsCommonPackArgs
        member __.AsServicesPack = this.AsServicesPack
    member __.AsCommonPack = this :> ICommonPack
    interface IAppPack with
        member __.Args = this.Args.AsAppPackArgs
        member __.AsCommonPack = this.AsCommonPack
        member __.AsServicesPack = this.AsServicesPack
    member __.AsAppPack = this :> IAppPack
    interface IBackupPack with
        member __.Args = this.Args.AsBackupPackArgs
        member __.BackupTicker (* IBackupPack *) : TickerTypes.Agent = backupTicker |> Option.get
        member __.AsCommonPack = this.AsCommonPack
    member __.AsBackupPack = this :> IBackupPack
    interface IApp with
        member __.Args : AppArgs = this.Args
        member __.AsAppPack = this.AsAppPack
        member __.AsBackupPack = this.AsBackupPack
    member __.AsApp = this :> IApp
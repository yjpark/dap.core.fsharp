[<AutoOpen>]
module Dap.Platform.Demo.App

open System.Threading.Tasks
open FSharp.Control.Tasks.V2
open Dap.Prelude
open Dap.Context
open Dap.Context.Builder
open Dap.Platform

module TickerTypes = Dap.Platform.Ticker.Types

type IServicesPackArgs =
    abstract Ticker : TickerTypes.Args with get

type IServicesPack =
    abstract Args : IServicesPackArgs with get
    abstract Ticker : IAgent<TickerTypes.Req, TickerTypes.Evt> with get

type IAppPackArgs =
    inherit IServicesPackArgs
    abstract Test : int with get

type IAppPack =
    abstract Args : IAppPackArgs with get
    inherit IServicesPack

type IBackupPackArgs =
    abstract BackupTicker : TickerTypes.Args with get

type IBackupPack =
    abstract Args : IBackupPackArgs with get
    abstract BackupTicker : IAgent<TickerTypes.Req, TickerTypes.Evt> with get

(*
 * Generated: <Record>
 *     IsJson, IsLoose
 *)
type AppArgs = {
    Ticker : (* IServicesPack *) TickerTypes.Args
    Test : (* IAppPack *) int
    BackupTicker : (* IBackupPack *) TickerTypes.Args
} with
    static member Create ticker test backupTicker
            : AppArgs =
        {
            Ticker = ticker
            Test = test
            BackupTicker = backupTicker
        }
    static member Default () =
        AppArgs.Create
            (TickerTypes.Args.Default ())
            100
            (decodeJson TickerTypes.Args.JsonDecoder """{"frame_rate":1,"auto_start":true}""")
    static member JsonEncoder : JsonEncoder<AppArgs> =
        fun (this : AppArgs) ->
            E.object [
                "ticker", TickerTypes.Args.JsonEncoder this.Ticker
            ]
    static member JsonDecoder : JsonDecoder<AppArgs> =
        D.decode AppArgs.Create
        |> D.optional "ticker" TickerTypes.Args.JsonDecoder (TickerTypes.Args.Default ())
        |> D.hardcoded 100
        |> D.hardcoded (decodeJson TickerTypes.Args.JsonDecoder """{"frame_rate":1,"auto_start":true}""")
    static member JsonSpec =
        FieldSpec.Create<AppArgs>
            AppArgs.JsonEncoder AppArgs.JsonDecoder
    interface IJson with
        member this.ToJson () = AppArgs.JsonEncoder this
    interface IObj
    member this.WithTicker ((* IServicesPack *) ticker : TickerTypes.Args) = {this with Ticker = ticker}
    member this.WithTest ((* IAppPack *) test : int) = {this with Test = test}
    member this.WithBackupTicker ((* IBackupPack *) backupTicker : TickerTypes.Args) = {this with BackupTicker = backupTicker}
    interface IAppPackArgs with
        member this.Test (* IAppPack *) : int = this.Test
    interface IServicesPackArgs with
        member this.Ticker (* IServicesPack *) : TickerTypes.Args = this.Ticker
    member this.AsServicesPackArgs = this :> IServicesPackArgs
    member this.AsAppPackArgs = this :> IAppPackArgs
    interface IBackupPackArgs with
        member this.BackupTicker (* IBackupPack *) : TickerTypes.Args = this.BackupTicker
    member this.AsBackupPackArgs = this :> IBackupPackArgs

(*
 * Generated: <ValueBuilder>
 *)
type AppArgsBuilder () =
    inherit ObjBuilder<AppArgs> ()
    override __.Zero () = AppArgs.Default ()
    [<CustomOperation("ticker")>]
    member __.Ticker (target : AppArgs, (* IServicesPack *) ticker : TickerTypes.Args) =
        target.WithTicker ticker
    [<CustomOperation("test")>]
    member __.Test (target : AppArgs, (* IAppPack *) test : int) =
        target.WithTest test
    [<CustomOperation("backup_ticker")>]
    member __.BackupTicker (target : AppArgs, (* IBackupPack *) backupTicker : TickerTypes.Args) =
        target.WithBackupTicker backupTicker

let appArgs = AppArgsBuilder ()

type IApp =
    inherit ILogger
    abstract Env : IEnv with get
    abstract Args : AppArgs with get
    abstract OnSetup : IBus<bool> with get
    inherit IAppPack
    inherit IBackupPack

type App (logging : ILogging, scope : Scope) =
    let env = Env.live MailboxPlatform logging scope
    let mutable args : AppArgs option = None
    let mutable setupResult : bool = false
    let onSetup = new Bus<bool> (env, "OnSetup")
    let mutable (* IServicesPack *) ticker : IAgent<TickerTypes.Req, TickerTypes.Evt> option = None
    let mutable (* IBackupPack *) backupTicker : IAgent<TickerTypes.Req, TickerTypes.Evt> option = None
    static member Create logging scope = new App (logging, scope)
    abstract member SetupExtrasAsync : unit -> Task<unit>
    default __.SetupExtrasAsync () = task {
        return ()
    }
    member this.SetupAsync (getArgs : unit -> AppArgs) : Task<bool> = task {
        if args.IsNone then
            let args' = getArgs ()
            args <- Some args'
            setupResult <- false
            try
                let! (* IServicesPack *) ticker' = env |> Env.addServiceAsync (Dap.Platform.Ticker.Logic.spec args'.Ticker) "Ticker" ""
                ticker <- Some (ticker' :> IAgent<TickerTypes.Req, TickerTypes.Evt>)
                let! (* IBackupPack *) backupTicker' = env |> Env.addServiceAsync (Dap.Platform.Ticker.Logic.spec args'.BackupTicker) "Ticker" "Backup"
                backupTicker <- Some (backupTicker' :> IAgent<TickerTypes.Req, TickerTypes.Evt>)
                do! this.SetupExtrasAsync ()
                setupResult <- true
                logInfo env "App.SetupAsync" "Setup_Succeed" (E.encodeJson 4 args')
            with e ->
                logException env "App.SetupAsync" "Setup_Failed" (E.encodeJson 4 args') e
            onSetup.Trigger setupResult
            return setupResult
        else
            logError env "App.SetupAsync" "Already_Setup" (args, setupResult, getArgs)
            return setupResult
    }
    member this.SetupAsync (args' : AppArgs) : Task<bool> =
        fun () -> args'
        |> this.SetupAsync
    member this.SetupAsync (args' : Json) : Task<bool> =
        fun () ->
            try
                castJson AppArgs.JsonDecoder args'
            with e ->
                logException env "App.SetupAsync" "Decode_Failed" args e
                raise e
        |> this.SetupAsync
    member this.SetupAsync (args' : string) : Task<bool> =
        let args' : Json = parseJson args'
        this.SetupAsync args'
    member __.SetupResult : bool = setupResult
    interface IApp with
        member __.Env : IEnv = env
        member __.Args : AppArgs = args |> Option.get
        member __.OnSetup : IBus<bool> = onSetup.Publish
    interface IAppPack with
        member __.Args = (Option.get args) .AsAppPackArgs
    interface IServicesPack with
        member __.Args = (Option.get args) .AsServicesPackArgs
        member __.Ticker (* IServicesPack *) : IAgent<TickerTypes.Req, TickerTypes.Evt> = ticker |> Option.get
    member this.AsServicesPack = this :> IServicesPack
    member this.AsAppPack = this :> IAppPack
    interface IBackupPack with
        member __.Args = (Option.get args) .AsBackupPackArgs
        member __.BackupTicker (* IBackupPack *) : IAgent<TickerTypes.Req, TickerTypes.Evt> = backupTicker |> Option.get
    member this.AsBackupPack = this :> IBackupPack
    interface ILogger with
        member __.Log m = env.Log m
    member this.AsApp = this :> IApp
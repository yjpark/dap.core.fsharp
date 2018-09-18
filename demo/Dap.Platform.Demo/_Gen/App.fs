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
    inherit ILogger
    abstract Env : IEnv with get
    abstract Args : IServicesPackArgs with get
    abstract Ticker : TickerTypes.Agent with get

type IAppPackArgs =
    inherit IServicesPackArgs
    abstract Test : int with get

type IAppPack =
    inherit ILogger
    inherit IServicesPack
    abstract Env : IEnv with get
    abstract Args : IAppPackArgs with get

type IBackupPackArgs =
    abstract BackupTicker : TickerTypes.Args with get

type IBackupPack =
    inherit ILogger
    abstract Env : IEnv with get
    abstract Args : IBackupPackArgs with get
    abstract BackupTicker : TickerTypes.Agent with get

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
            (decodeJsonValue TickerTypes.Args.JsonDecoder """{"frame_rate":1,"auto_start":true}""")
    static member JsonEncoder : JsonEncoder<AppArgs> =
        fun (this : AppArgs) ->
            E.object [
                "ticker", TickerTypes.Args.JsonEncoder this.Ticker
            ]
    static member JsonDecoder : JsonDecoder<AppArgs> =
        D.decode AppArgs.Create
        |> D.optional "ticker" TickerTypes.Args.JsonDecoder (TickerTypes.Args.Default ())
        |> D.hardcoded 100
        |> D.hardcoded (decodeJsonValue TickerTypes.Args.JsonDecoder """{"frame_rate":1,"auto_start":true}""")
    static member JsonSpec =
        FieldSpec.Create<AppArgs>
            AppArgs.JsonEncoder AppArgs.JsonDecoder
    interface IJson with
        member this.ToJson () = AppArgs.JsonEncoder this
    interface IObj
    member this.WithTicker ((* IServicesPack *) ticker : TickerTypes.Args) = {this with Ticker = ticker}
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

let appArgs = AppArgsBuilder ()

type IApp =
    inherit ILogger
    abstract LoggingArgs : LoggingArgs with get
    abstract Env : IEnv with get
    abstract Args : AppArgs with get
    inherit IAppPack
    inherit IBackupPack

type App (loggingArgs : LoggingArgs, scope : Scope) =
    let env = Env.live MailboxPlatform (loggingArgs.CreateLogging ()) scope
    let mutable args : AppArgs option = None
    let mutable setupError : exn option = None
    let mutable (* IServicesPack *) ticker : TickerTypes.Agent option = None
    let mutable (* IBackupPack *) backupTicker : TickerTypes.Agent option = None
    abstract member SetupExtrasAsync : unit -> Task<unit>
    default __.SetupExtrasAsync () = task {
        return ()
    }
    member this.SetupAsync (getArgs : unit -> AppArgs) : Task<unit> = task {
        if args.IsNone then
            let args' = getArgs ()
            args <- Some args'
            try
                let! (* IServicesPack *) ticker' = env |> Env.addServiceAsync (Dap.Platform.Ticker.Logic.spec args'.Ticker) "Ticker" ""
                ticker <- Some ticker'
                let! (* IBackupPack *) backupTicker' = env |> Env.addServiceAsync (Dap.Platform.Ticker.Logic.spec args'.BackupTicker) "Ticker" "Backup"
                backupTicker <- Some backupTicker'
                do! this.SetupExtrasAsync ()
                logInfo env "App.SetupAsync" "Setup_Succeed" (E.encodeJson 4 args')
            with e ->
                setupError <- Some e
                logException env "App.SetupAsync" "Setup_Failed" (E.encodeJson 4 args') e
        else
            logError env "App.SetupAsync" "Already_Setup" (args, setupError, getArgs)
    }
    member this.Setup (callback : IApp -> unit) (getArgs : unit -> AppArgs) : IApp =
        if args.IsSome then
            failWith "Already_Setup" <| E.encodeJson 4 args.Value
        env.RunTask0 raiseOnFailed (fun _ -> task {
            do! this.SetupAsync getArgs
            match setupError with
            | None -> callback this.AsApp
            | Some e -> raise e
        })
        this.AsApp
    member this.SetupArgs (callback : IApp -> unit) (args' : AppArgs) : IApp =
        fun () -> args'
        |> this.Setup callback
    member this.SetupJson (callback : IApp -> unit) (args' : Json) : IApp =
        fun () ->
            try
                castJson AppArgs.JsonDecoder args'
            with e ->
                logException env "App.Setup" "Decode_Failed" args e
                raise e
        |> this.Setup callback
    member this.SetupText (callback : IApp -> unit) (args' : string) : IApp =
        parseJson args'
        |> this.SetupJson callback
    member __.SetupError : exn option = setupError
    member __.Args : AppArgs = args |> Option.get
    interface IApp with
        member __.LoggingArgs : LoggingArgs = loggingArgs
        member __.Env : IEnv = env
        member this.Args : AppArgs = this.Args
    interface IAppPack with
        member __.Env : IEnv = env
        member this.Args = this.Args.AsAppPackArgs
    interface IServicesPack with
        member __.Env : IEnv = env
        member this.Args = this.Args.AsServicesPackArgs
        member __.Ticker (* IServicesPack *) : TickerTypes.Agent = ticker |> Option.get
    member this.AsServicesPack = this :> IServicesPack
    member this.AsAppPack = this :> IAppPack
    interface IBackupPack with
        member __.Env : IEnv = env
        member this.Args = this.Args.AsBackupPackArgs
        member __.BackupTicker (* IBackupPack *) : TickerTypes.Agent = backupTicker |> Option.get
    member this.AsBackupPack = this :> IBackupPack
    interface ILogger with
        member __.Log m = env.Log m
    member this.AsApp = this :> IApp
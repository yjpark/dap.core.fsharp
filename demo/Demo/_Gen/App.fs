[<AutoOpen>]
module Demo.App

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
    inherit IPack
    abstract Args : IServicesPackArgs with get
    abstract Ticker : TickerTypes.Agent with get

type ICommonPackArgs =
    inherit IServicesPackArgs
    abstract Common : int with get
    abstract AsServicesPackArgs : IServicesPackArgs with get

type ICommonPack =
    inherit IPack
    inherit IServicesPack
    abstract Args : ICommonPackArgs with get
    abstract AsServicesPack : IServicesPack with get

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
 * Generated: <Record>
 *     IsJson, IsLoose
 *)
type AppArgs = {
    Ticker : (* IServicesPack *) TickerTypes.Args
    Common : (* ICommonPack *) int
    Test : (* IAppPack *) int
    BackupTicker : (* IBackupPack *) TickerTypes.Args
} with
    static member Create ticker common test backupTicker
            : AppArgs =
        {
            Ticker = ticker
            Common = common
            Test = test
            BackupTicker = backupTicker
        }
    static member Default () =
        AppArgs.Create
            (TickerTypes.Args.Default ())
            100
            100
            (decodeJsonValue TickerTypes.Args.JsonDecoder """{"frame_rate":1.0,"auto_start":true}""")
    static member SetTicker ((* IServicesPack *) ticker : TickerTypes.Args) (this : AppArgs) =
        {this with Ticker = ticker}
    static member SetCommon ((* ICommonPack *) common : int) (this : AppArgs) =
        {this with Common = common}
    static member SetTest ((* IAppPack *) test : int) (this : AppArgs) =
        {this with Test = test}
    static member SetBackupTicker ((* IBackupPack *) backupTicker : TickerTypes.Args) (this : AppArgs) =
        {this with BackupTicker = backupTicker}
    static member UpdateTicker ((* IServicesPack *) update : TickerTypes.Args -> TickerTypes.Args) (this : AppArgs) =
        this |> AppArgs.SetTicker (update this.Ticker)
    static member UpdateCommon ((* ICommonPack *) update : int -> int) (this : AppArgs) =
        this |> AppArgs.SetCommon (update this.Common)
    static member UpdateTest ((* IAppPack *) update : int -> int) (this : AppArgs) =
        this |> AppArgs.SetTest (update this.Test)
    static member UpdateBackupTicker ((* IBackupPack *) update : TickerTypes.Args -> TickerTypes.Args) (this : AppArgs) =
        this |> AppArgs.SetBackupTicker (update this.BackupTicker)
    static member JsonEncoder : JsonEncoder<AppArgs> =
        fun (this : AppArgs) ->
            E.object [
                "ticker", TickerTypes.Args.JsonEncoder this.Ticker
            ]
    static member JsonDecoder : JsonDecoder<AppArgs> =
        D.decode AppArgs.Create
        |> D.optional "ticker" TickerTypes.Args.JsonDecoder (TickerTypes.Args.Default ())
        |> D.hardcoded 100
        |> D.hardcoded 100
        |> D.hardcoded (decodeJsonValue TickerTypes.Args.JsonDecoder """{"frame_rate":1.0,"auto_start":true}""")
    static member JsonSpec =
        FieldSpec.Create<AppArgs>
            AppArgs.JsonEncoder AppArgs.JsonDecoder
    interface IJson with
        member this.ToJson () = AppArgs.JsonEncoder this
    interface IObj
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
    [<CustomOperation("ticker")>]
    member __.Ticker (target : AppArgs, (* IServicesPack *) ticker : TickerTypes.Args) =
        target.WithTicker ticker

let appArgs = AppArgsBuilder ()

type IApp =
    inherit IPack
    inherit IAppPack
    inherit IBackupPack
    abstract Args : AppArgs with get
    abstract AsAppPack : IAppPack with get
    abstract AsBackupPack : IBackupPack with get

type AppKinds () =
    static member Ticker (* IServicesPack *) = "Ticker"
    static member BackupTicker (* IBackupPack *) = "Ticker"

type AppKeys () =
    static member Ticker (* IServicesPack *) = ""
    static member BackupTicker (* IBackupPack *) = "Backup"

type App (logging : ILogging, scope : Scope) =
    let env = Env.live MailboxPlatform logging scope
    let mutable args : AppArgs option = None
    let mutable setupError : exn option = None
    let mutable (* IServicesPack *) ticker : TickerTypes.Agent option = None
    let mutable (* IBackupPack *) backupTicker : TickerTypes.Agent option = None
    let setupAsync (this : App) : Task<unit> = task {
        let args' = args |> Option.get
        try
            let! (* IServicesPack *) ticker' = env |> Env.addServiceAsync (Dap.Platform.Ticker.Logic.spec args'.Ticker) AppKinds.Ticker AppKeys.Ticker
            ticker <- Some ticker'
            let! (* IBackupPack *) backupTicker' = env |> Env.addServiceAsync (Dap.Platform.Ticker.Logic.spec args'.BackupTicker) AppKinds.BackupTicker AppKeys.BackupTicker
            backupTicker <- Some backupTicker'
            do! this.SetupAsync' ()
            logInfo env "App.setupAsync" "Setup_Succeed" (E.encodeJson 4 args')
        with e ->
            setupError <- Some e
            logException env "App.setupAsync" "Setup_Failed" (E.encodeJson 4 args') e
    }
    new (loggingArgs : LoggingArgs, scope : Scope) =
        App (loggingArgs.CreateLogging (), scope)
    new (scope : Scope) =
        App (getLogging (), scope)
    member this.SetupAsync (getArgs : unit -> AppArgs) : Task<unit> = task {
        if args.IsSome then
            failWith "Already_Setup" <| E.encodeJson 4 args.Value
        else
            let args' = getArgs ()
            args <- Some args'
            do! setupAsync this
            match setupError with
            | None -> ()
            | Some e -> raise e
        return ()
        }
    member this.SetupAsync (args' : AppArgs) : Task<unit> =
        fun () -> args'
        |> this.SetupAsync
    member this.SetupAsync (args' : Json) : Task<unit> =
        fun () ->
            try
                castJson AppArgs.JsonDecoder args'
            with e ->
                logException env "App.SetupAsync" "Decode_Failed" args e
                raise e
        |> this.SetupAsync
    member this.SetupAsync (args' : string) : Task<unit> =
        let json : Json = parseJson args'
        this.SetupAsync json
    member __.SetupError : exn option = setupError
    abstract member SetupAsync' : unit -> Task<unit>
    default __.SetupAsync' () = task {
        return ()
    }
    member __.Args : AppArgs = args |> Option.get
    interface ILogger with
        member __.Log m = env.Log m
    interface IPack with
        member __.Env : IEnv = env
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
        member __.BackupTicker (* IBackupPack *) : TickerTypes.Agent = backupTicker |> Option.get
        member this.AsCommonPack = this.AsCommonPack
    member this.AsBackupPack = this :> IBackupPack
    interface IApp with
        member this.Args : AppArgs = this.Args
        member this.AsAppPack = this.AsAppPack
        member this.AsBackupPack = this.AsBackupPack
    member this.AsApp = this :> IApp
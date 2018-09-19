[<AutoOpen>]
module Dap.Platform.Demo.App

open Dap.Prelude
open Dap.Context
open Dap.Context.Builder
open Dap.Platform

type IServicesPackArgs =
    abstract Fake : NoArgs with get
    abstract Service : int with get

type IServicesPack =
    inherit IPack
    abstract Args : IServicesPackArgs with get
    abstract Fake : FakeService with get

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
    abstract View : NoArgs with get
    abstract Test : int with get
    abstract AsCommonPackArgs : ICommonPackArgs with get
    abstract AsServicesPackArgs : IServicesPackArgs with get

type IAppPack =
    inherit IPack
    inherit ICommonPack
    inherit IServicesPack
    abstract Args : IAppPackArgs with get
    abstract View : FakeView with get
    abstract AsCommonPack : ICommonPack with get
    abstract AsServicesPack : IServicesPack with get

type IBackupPackArgs =
    inherit ICommonPackArgs
    abstract AnotherFake : NoArgs with get
    abstract Backup : int with get
    abstract AsCommonPackArgs : ICommonPackArgs with get

type IBackupPack =
    inherit IPack
    inherit ICommonPack
    abstract Args : IBackupPackArgs with get
    abstract AnotherFake : FakeService with get
    abstract AsCommonPack : ICommonPack with get

(*
 * Generated: <Record>
 *     IsJson, IsLoose
 *)
type AppArgs = {
    Fake : (* IServicesPack *) NoArgs
    Service : (* IServicesPack *) int
    Common : (* ICommonPack *) int
    View : (* IAppPack *) NoArgs
    Test : (* IAppPack *) int
    AnotherFake : (* IBackupPack *) NoArgs
    Backup : (* IBackupPack *) int
} with
    static member Create fake service common view test anotherFake backup
            : AppArgs =
        {
            Fake = fake
            Service = service
            Common = common
            View = view
            Test = test
            AnotherFake = anotherFake
            Backup = backup
        }
    static member Default () =
        AppArgs.Create
            NoArgs
            2
            100
            NoArgs
            100
            NoArgs
            200
    static member JsonEncoder : JsonEncoder<AppArgs> =
        fun (this : AppArgs) ->
            E.object [
            ]
    static member JsonDecoder : JsonDecoder<AppArgs> =
        D.decode AppArgs.Create
        |> D.hardcoded NoArgs
        |> D.hardcoded 2
        |> D.hardcoded 100
        |> D.hardcoded NoArgs
        |> D.hardcoded 100
        |> D.hardcoded NoArgs
        |> D.hardcoded 200
    static member JsonSpec =
        FieldSpec.Create<AppArgs>
            AppArgs.JsonEncoder AppArgs.JsonDecoder
    interface IJson with
        member this.ToJson () = AppArgs.JsonEncoder this
    interface IObj
    interface IServicesPackArgs with
        member this.Fake (* IServicesPack *) : NoArgs = this.Fake
        member this.Service (* IServicesPack *) : int = this.Service
    member this.AsServicesPackArgs = this :> IServicesPackArgs
    interface ICommonPackArgs with
        member this.Common (* ICommonPack *) : int = this.Common
        member this.AsServicesPackArgs = this.AsServicesPackArgs
    member this.AsCommonPackArgs = this :> ICommonPackArgs
    interface IAppPackArgs with
        member this.View (* IAppPack *) : NoArgs = this.View
        member this.Test (* IAppPack *) : int = this.Test
        member this.AsCommonPackArgs = this.AsCommonPackArgs
        member this.AsServicesPackArgs = this.AsServicesPackArgs
    member this.AsAppPackArgs = this :> IAppPackArgs
    interface IBackupPackArgs with
        member this.AnotherFake (* IBackupPack *) : NoArgs = this.AnotherFake
        member this.Backup (* IBackupPack *) : int = this.Backup
        member this.AsCommonPackArgs = this.AsCommonPackArgs
    member this.AsBackupPackArgs = this :> IBackupPackArgs

(*
 * Generated: <ValueBuilder>
 *)
type AppArgsBuilder () =
    inherit ObjBuilder<AppArgs> ()
    override __.Zero () = AppArgs.Default ()

let appArgs = AppArgsBuilder ()

type IApp =
    inherit IPack
    inherit IAppPack
    inherit IBackupPack
    abstract Args : AppArgs with get
    abstract AsAppPack : IAppPack with get
    abstract AsBackupPack : IBackupPack with get

type App (logging : ILogging, scope : Scope) =
    let env = Env.live logging scope
    let mutable args : AppArgs option = None
    let mutable setupError : exn option = None
    let mutable (* IServicesPack *) fake : FakeService option = None
    let mutable (* IAppPack *) view : FakeView option = None
    let mutable (* IBackupPack *) anotherFake : FakeService option = None
    let setup (this : App) : unit =
        let args' = args |> Option.get
        try
            let (* IServicesPack *) fake' = env |> Env.spawn (FakeService.spec args'.Fake) "Fake" ""
            fake <- Some fake'
            let (* IAppPack *) view' = env |> Env.spawn (FakeView.spec this.AsBackupPack args'.View) "View" ""
            view <- Some view'
            let (* IBackupPack *) anotherFake' = env |> Env.spawn (FakeService.spec args'.AnotherFake) "Fake" "Another"
            anotherFake <- Some anotherFake'
            this.Setup' ()
            logInfo env "App.setup" "Setup_Succeed" (E.encodeJson 4 args')
        with e ->
            setupError <- Some e
            logException env "App.setup" "Setup_Failed" (E.encodeJson 4 args') e
    new (scope : Scope) =
        App (getLogging (), scope)
    member this.Setup (callback : IApp -> unit) (getArgs : unit -> AppArgs) : IApp =
        if args.IsSome then
            failWith "Already_Setup" <| E.encodeJson 4 args.Value
        else
            let args' = getArgs ()
            args <- Some args'
            setup this
            match setupError with
            | None -> callback this.AsApp
            | Some e -> raise e
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
    abstract member Setup' : unit -> unit
    default __.Setup' () = ()
    member __.Args : AppArgs = args |> Option.get
    interface ILogger with
        member __.Log m = env.Log m
    interface IPack with
        member __.Env : IEnv = env
    interface IServicesPack with
        member this.Args = this.Args.AsServicesPackArgs
        member __.Fake (* IServicesPack *) : FakeService = fake |> Option.get
    member this.AsServicesPack = this :> IServicesPack
    interface ICommonPack with
        member this.Args = this.Args.AsCommonPackArgs
        member this.AsServicesPack = this.AsServicesPack
    member this.AsCommonPack = this :> ICommonPack
    interface IAppPack with
        member this.Args = this.Args.AsAppPackArgs
        member __.View (* IAppPack *) : FakeView = view |> Option.get
        member this.AsCommonPack = this.AsCommonPack
        member this.AsServicesPack = this.AsServicesPack
    member this.AsAppPack = this :> IAppPack
    interface IBackupPack with
        member this.Args = this.Args.AsBackupPackArgs
        member __.AnotherFake (* IBackupPack *) : FakeService = anotherFake |> Option.get
        member this.AsCommonPack = this.AsCommonPack
    member this.AsBackupPack = this :> IBackupPack
    interface IApp with
        member this.Args : AppArgs = this.Args
        member this.AsAppPack = this.AsAppPack
        member this.AsBackupPack = this.AsBackupPack
    member this.AsApp = this :> IApp
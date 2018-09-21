[<AutoOpen>]
module Demo.App

open Dap.Prelude
open Dap.Context
open Dap.Context.Builder
open Dap.Platform

module Proxy = Dap.Remote.WebSocketProxy.Proxy
module UserHubTypes = Demo.UserHub.Types

type IClientPackArgs =
    abstract UserStub' : Proxy.Args<UserHubTypes.Req, UserHubTypes.ClientRes, UserHubTypes.Evt> with get

type IClientPack =
    inherit IPack
    abstract Args : IClientPackArgs with get
    abstract UserStub : Proxy.Proxy<UserHubTypes.Req, UserHubTypes.ClientRes, UserHubTypes.Evt> with get

type IAppPackArgs =
    inherit IClientPackArgs
    abstract AsClientPackArgs : IClientPackArgs with get

type IAppPack =
    inherit IPack
    inherit IClientPack
    abstract Args : IAppPackArgs with get
    abstract AsClientPack : IClientPack with get

(*
 * Generated: <Record>
 *     IsJson, IsLoose
 *)
type AppArgs = {
    UserStub : (* IClientPack *) Proxy.Args<UserHubTypes.Req, UserHubTypes.ClientRes, UserHubTypes.Evt>
} with
    static member Create userStub
            : AppArgs =
        {
            UserStub = userStub
        }
    static member Default () =
        AppArgs.Create
            (Proxy.args UserHubTypes.StubSpec (getWebSocketUri "ws_user") (Some 5.000000<second>) true)
    static member SetUserStub ((* IClientPack *) userStub : Proxy.Args<UserHubTypes.Req, UserHubTypes.ClientRes, UserHubTypes.Evt>) (this : AppArgs) =
        {this with UserStub = userStub}
    static member UpdateUserStub ((* IClientPack *) update : Proxy.Args<UserHubTypes.Req, UserHubTypes.ClientRes, UserHubTypes.Evt> -> Proxy.Args<UserHubTypes.Req, UserHubTypes.ClientRes, UserHubTypes.Evt>) (this : AppArgs) =
        this |> AppArgs.SetUserStub (update this.UserStub)
    static member JsonEncoder : JsonEncoder<AppArgs> =
        fun (this : AppArgs) ->
            E.object []
    static member JsonDecoder : JsonDecoder<AppArgs> =
        D.decode AppArgs.Create
        |> D.hardcoded (Proxy.args UserHubTypes.StubSpec (getWebSocketUri "ws_user") (Some 5.000000<second>) true)
    static member JsonSpec =
        FieldSpec.Create<AppArgs>
            AppArgs.JsonEncoder AppArgs.JsonDecoder
    interface IJson with
        member this.ToJson () = AppArgs.JsonEncoder this
    interface IObj
    member this.WithUserStub ((* IClientPack *) userStub : Proxy.Args<UserHubTypes.Req, UserHubTypes.ClientRes, UserHubTypes.Evt>) =
        this |> AppArgs.SetUserStub userStub
    interface IClientPackArgs with
        member this.UserStub' (* IClientPack *) : Proxy.Args<UserHubTypes.Req, UserHubTypes.ClientRes, UserHubTypes.Evt> = this.UserStub
    member this.AsClientPackArgs = this :> IClientPackArgs
    interface IAppPackArgs with
        member this.AsClientPackArgs = this.AsClientPackArgs
    member this.AsAppPackArgs = this :> IAppPackArgs

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
    abstract Args : AppArgs with get
    abstract AsAppPack : IAppPack with get

type App (logging : ILogging, scope : Scope) =
    let env = Env.live logging scope
    let mutable args : AppArgs option = None
    let mutable setupError : exn option = None
    let mutable (* IClientPack *) userStub : Proxy.Proxy<UserHubTypes.Req, UserHubTypes.ClientRes, UserHubTypes.Evt> option = None
    let setup (this : App) : unit =
        let args' = args |> Option.get
        try
            let (* IClientPack *) userStub' = env |> Env.spawn (Dap.Remote.Proxy.Logic.spec args'.UserStub) "UserStub" ""
            userStub <- Some userStub'
            this.Setup' ()
            logInfo env "App.setup" "Setup_Succeed" (E.encodeJson 4 args')
        with e ->
            setupError <- Some e
            logException env "App.setup" "Setup_Failed" (E.encodeJson 4 args') e
    new (scope : Scope) =
        App (getLogging (), scope)
    member this.Setup (getArgs : unit -> AppArgs) : unit =
        if args.IsSome then
            failWith "Already_Setup" <| E.encodeJson 4 args.Value
        else
            let args' = getArgs ()
            args <- Some args'
            setup this
            match setupError with
            | None -> ()
            | Some e -> raise e
    member this.Setup (args' : AppArgs) : unit =
        fun () -> args'
        |> this.Setup
    member this.Setup (args' : Json) : unit =
        fun () ->
            try
                castJson AppArgs.JsonDecoder args'
            with e ->
                logException env "App.Setup" "Decode_Failed" args e
                raise e
        |> this.Setup
    member this.Setup (args' : string) : unit =
        let json : Json = parseJson args'
        this.Setup json
    member __.SetupError : exn option = setupError
    abstract member Setup' : unit -> unit
    default __.Setup' () = ()
    member __.Args : AppArgs = args |> Option.get
    interface ILogger with
        member __.Log m = env.Log m
    interface IPack with
        member __.Env : IEnv = env
    interface IClientPack with
        member this.Args = this.Args.AsClientPackArgs
        member __.UserStub (* IClientPack *) : Proxy.Proxy<UserHubTypes.Req, UserHubTypes.ClientRes, UserHubTypes.Evt> = userStub |> Option.get
    member this.AsClientPack = this :> IClientPack
    interface IAppPack with
        member this.Args = this.Args.AsAppPackArgs
        member this.AsClientPack = this.AsClientPack
    member this.AsAppPack = this :> IAppPack
    interface IApp with
        member this.Args : AppArgs = this.Args
        member this.AsAppPack = this.AsAppPack
    member this.AsApp = this :> IApp
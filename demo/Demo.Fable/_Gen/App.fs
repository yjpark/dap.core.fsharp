[<AutoOpen>]
module Demo.App

open Dap.Prelude
open Dap.Context
open Dap.Context.Builder
open Dap.Platform

module Proxy = Dap.Remote.WebSocketProxy.Proxy
module UserHubTypes = Demo.UserHub.Types

(*
 * Generated: <Pack>
 *)
type IClientPackArgs =
    abstract UserStub' : Proxy.Args<UserHubTypes.Req, UserHubTypes.ClientRes, UserHubTypes.Evt> with get

type IClientPack =
    inherit IPack
    abstract UserStub : Proxy.Proxy<UserHubTypes.Req, UserHubTypes.ClientRes, UserHubTypes.Evt> with get

(*
 * Generated: <Pack>
 *)
type IAppPackArgs =
    inherit IClientPackArgs
    abstract AsClientPackArgs : IClientPackArgs with get

type IAppPack =
    inherit IPack
    inherit IClientPack
    abstract AsClientPack : IClientPack with get

(*
 * Generated: <App>
 *)

type AppKinds () =
    static member UserStub (* IClientPack *) = "UserStub"

type AppKeys () =
    static member UserStub (* IClientPack *) = ""

type IApp =
    inherit IPack
    inherit IAppPack
    abstract Args : AppArgs with get
    abstract AsAppPack : IAppPack with get

(*
 * Generated: <Record>
 *     IsJson, IsLoose
 *)
and AppArgs = {
    Scope : (* AppArgs *) Scope
    Setup : (* AppArgs *) IApp -> unit
    UserStub : (* IClientPack *) Proxy.Args<UserHubTypes.Req, UserHubTypes.ClientRes, UserHubTypes.Evt>
} with
    static member Create
        (
            ?scope : Scope,
            ?setup : IApp -> unit,
            ?userStub : Proxy.Args<UserHubTypes.Req, UserHubTypes.ClientRes, UserHubTypes.Evt>
        ) : AppArgs =
        {
            Scope = (* AppArgs *) scope
                |> Option.defaultWith (fun () -> NoScope)
            Setup = (* AppArgs *) setup
                |> Option.defaultWith (fun () -> ignore)
            UserStub = (* IClientPack *) userStub
                |> Option.defaultWith (fun () -> (Proxy.args UserHubTypes.StubSpec (getWebSocketUri "ws_user") (Some 5.000000<second>) true))
        }
    static member Default () =
        AppArgs.Create (
            NoScope, (* AppArgs *) (* scope *)
            ignore, (* AppArgs *) (* setup *)
            (Proxy.args UserHubTypes.StubSpec (getWebSocketUri "ws_user") (Some 5.000000<second>) true) (* IClientPack *) (* userStub *)
        )
    static member SetScope ((* AppArgs *) scope : Scope) (this : AppArgs) =
        {this with Scope = scope}
    static member SetSetup ((* AppArgs *) setup : IApp -> unit) (this : AppArgs) =
        {this with Setup = setup}
    static member SetUserStub ((* IClientPack *) userStub : Proxy.Args<UserHubTypes.Req, UserHubTypes.ClientRes, UserHubTypes.Evt>) (this : AppArgs) =
        {this with UserStub = userStub}
    static member JsonEncoder : JsonEncoder<AppArgs> =
        fun (this : AppArgs) ->
            E.object [
                "scope", Scope.JsonEncoder (* AppArgs *) this.Scope
            ]
    static member JsonDecoder : JsonDecoder<AppArgs> =
        D.object (fun get ->
            {
                Scope = get.Optional.Field (* AppArgs *) "scope" Scope.JsonDecoder
                    |> Option.defaultValue NoScope
                Setup = (* (* AppArgs *)  *) ignore
                UserStub = (* (* IClientPack *)  *) (Proxy.args UserHubTypes.StubSpec (getWebSocketUri "ws_user") (Some 5.000000<second>) true)
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
    [<CustomOperation("scope")>]
    member __.Scope (target : AppArgs, (* AppArgs *) scope : Scope) =
        target.WithScope scope
    [<CustomOperation("Setup")>]
    member __.Setup (target : AppArgs, (* AppArgs *) setup : IApp -> unit) =
        target.WithSetup setup
    [<CustomOperation("user_stub")>]
    member __.UserStub (target : AppArgs, (* IClientPack *) userStub : Proxy.Args<UserHubTypes.Req, UserHubTypes.ClientRes, UserHubTypes.Evt>) =
        target.WithUserStub userStub

let app_args = AppArgsBuilder ()

(*
 * Generated: <App>
 *)
type App (logging : ILogging, args : AppArgs) as this =
    let env = Env.live logging args.Scope
    let mutable setupError : exn option = None
    let mutable (* IClientPack *) userStub : Proxy.Proxy<UserHubTypes.Req, UserHubTypes.ClientRes, UserHubTypes.Evt> option = None
    let setup () : unit =
        try
            let (* IClientPack *) userStub' = env |> Env.spawn (Dap.Remote.Proxy.Logic.Logic.spec args.UserStub) AppKinds.UserStub AppKeys.UserStub
            userStub <- Some userStub'
            this.Setup' ()
            logInfo env "App.setup" "Setup_Succeed" (encodeJson 4 args)
            args.Setup this.AsApp
        with e ->
            setupError <- Some e
            logException env "App.setup" "Setup_Failed" (encodeJson 4 args) e
    do (
        setup ()
    )
    abstract member Setup' : unit -> unit
    default __.Setup' () = ()
    member __.Args : AppArgs = args
    member __.Env : IEnv = env
    member __.SetupError : exn option = setupError
    interface ILogger with
        member __.Log m = env.Log m
    interface IPack with
        member __.Env : IEnv = env
    interface IClientPack with
        member __.UserStub (* IClientPack *) : Proxy.Proxy<UserHubTypes.Req, UserHubTypes.ClientRes, UserHubTypes.Evt> = userStub |> Option.get
    member __.AsClientPack = this :> IClientPack
    interface IAppPack with
        member __.AsClientPack = this.AsClientPack
    member __.AsAppPack = this :> IAppPack
    interface IApp with
        member __.Args : AppArgs = this.Args
        member __.AsAppPack = this.AsAppPack
    member __.AsApp = this :> IApp
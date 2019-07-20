[<AutoOpen>]
module Dap.Platform.Features

open System.Threading.Tasks

open Dap.Prelude
open Dap.Context

[<Literal>]
let PlatformKind = "Platform"

[<Literal>]
let AppRunnerKind = "AppRunner"

type IAppRunner =
    inherit IFeature
    abstract Start<'app when 'app :> IBaseApp> : 'app -> unit
    abstract StartAsync<'app when 'app :> IBaseApp> : 'app -> Task<unit>

[<AbstractClass>]
type BasePlatform (logging : ILogging) =
    inherit EmptyContext (logging, PlatformKind)
    abstract member Start : IRunnable<'initer, 'runner, 'args, 'model, 'msg> -> unit
    interface IPlatform with
        member this.Start runnable = this.Start runnable

[<AbstractClass>]
type BaseAppRunner (logging : ILogging) =
    inherit EmptyContext (logging, AppRunnerKind)
    abstract member StartAsync<'app when 'app :> IBaseApp> : 'app -> Task<unit>
    abstract member Start<'app when 'app :> IBaseApp> : 'app -> unit
    default this.Start app =
        this.StartAsync app
        |> Async.AwaitTask
        |> Async.RunSynchronously
    interface IAppRunner with
        member this.Start app = this.Start app
        member this.StartAsync app = this.StartAsync app

type AppRunner (logging) =
    inherit BaseAppRunner (logging)
    override this.StartAsync app =
        app.SetupAsync ()
    interface IFallback
[<AutoOpen>]
module Dap.Platform.Features

open Dap.Prelude
open Dap.Context

[<Literal>]
let PlatformKind = "Platform"

[<Literal>]
let AppRunnerKind = "AppRunner"

type IAppRunner =
    inherit IFeature
    abstract Start : IApp<'app> -> unit

[<AbstractClass>]
type BasePlatform (logging : ILogging) =
    inherit EmptyContext (logging, PlatformKind)
    abstract member Start : IRunnable<'initer, 'runner, 'args, 'model, 'msg> -> unit
    interface IPlatform with
        member this.Start runnable = this.Start runnable

[<AbstractClass>]
type BaseAppRunner (logging : ILogging) =
    inherit EmptyContext (logging, AppRunnerKind)
    abstract member Start : IApp<'app> -> unit
    interface IAppRunner with
        member this.Start app = this.Start app

type AppRunner (logging) =
    inherit BaseAppRunner (logging)
    override this.Start app =
        app.RunTask raiseOnFailed (fun _ -> app.SetupAsync ())
    interface IFallback
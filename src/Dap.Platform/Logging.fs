[<AutoOpen>]
module Dap.Platform.Logging'

open Dap.Prelude
open Dap.Context

[<Literal>]
let ActorVersion = "Ver"

[<Literal>]
let LoggingProviderKind = "LoggingProvider"

let private forAgent (agent : IAgent<'args, 'model, 'msg, 'req, 'evt>) (logger : Serilog.ILogger) : Serilog.ILogger =
    { new Serilog.Core.ILogEventEnricher with
        member x.Enrich (logEvent, propertyFactory) =
            propertyFactory.CreateProperty (ActorVersion, agent.Actor.Version.ToString ())
            |> logEvent.AddOrUpdateProperty
    }
    |> logger.ForContext

let enrichLoggerForAgent (agent : IAgent<'args, 'model, 'msg, 'req, 'evt>) (logger : ILogger) =
    match logger with
    | :? SerilogLogger as logger ->
        new SerilogLogger
            (
                forAgent agent logger.Target,
                logger.MainTarget
                |> Option.map (forAgent agent)
            )
        :> ILogger
    | _ ->
        logError agent "Dap.Platform.Logging" "Enrich_Failed" <| logger.GetType ()
        logger

type ILoggingProvider =
    inherit IFeature
    abstract CreateLogging : LoggingArgs -> ILogging

[<AbstractClass>]
type BaseLoggingProvider (logging : ILogging) =
    inherit EmptyContext (logging, LoggingProviderKind)
    abstract member CreateLogging : LoggingArgs -> ILogging
    interface ILoggingProvider with
        member this.CreateLogging args = this.CreateLogging args

type LocalLoggingProvider (logging : ILogging) =
    inherit BaseLoggingProvider (logging)
    override this.CreateLogging (args : LoggingArgs) =
        let newArgs = args.WithFolder("log")
        let logging = newArgs.ToSerilogLogging ()
        logInfo logging "NetCoreRuntime" "CreateLogging" (encodeJson 4 newArgs)
        if newArgs.File.IsSome then
            let note = sprintf "%s -> %s", args.File.Value.Folder, newArgs.File.Value.Folder
            logInfo logging "NetCoreRuntime" "Folder_Updated" note
        logging :> ILogging
    interface IFallback


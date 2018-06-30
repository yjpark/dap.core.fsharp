[<AutoOpen>]
module Dap.Platform.Logging'

open Dap.Prelude

[<Literal>]
let ActorVersion = "Ver"

let private forAgent (agent : IAgent<'args, 'model, 'req, 'evt>) (logger : Serilog.ILogger) : Serilog.ILogger =
    { new Serilog.Core.ILogEventEnricher with
        member x.Enrich (logEvent, propertyFactory) =
            propertyFactory.CreateProperty (ActorVersion, agent.Actor.Version.ToString ())
            |> logEvent.AddOrUpdateProperty
    }
    |> logger.ForContext

let enrichLoggerForAgent (agent : IAgent<'args, 'model, 'req, 'evt>) (logger : ILogger) =
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
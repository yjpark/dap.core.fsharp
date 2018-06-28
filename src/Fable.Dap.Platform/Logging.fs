[<AutoOpen>]
module Dap.Platform.Logging'

open Dap.Prelude

[<Literal>]
let ActorVersion = "Ver"

let private enrichPrefixforAgent (agent : IAgent<'model, 'req, 'evt>) (prefix : string) : string =
    prefix + (agent.Actor.Version.ToString ())

let enrichLoggerForAgent (agent : IAgent<'model, 'req, 'evt>) (logger : ILogger) =
    match logger with
    | :? ConsoleLogger as logger ->
        new ConsoleLogger (logger.MinimumLevel, logger.Prefix,
            ConsoleLoggerEnricher.ForPrefix (enrichPrefixforAgent agent))
        :> ILogger
    | _ ->
        logError agent "Dap.Platform.Logging" "Enrich_Failed" <| logger.GetType ()
        logger
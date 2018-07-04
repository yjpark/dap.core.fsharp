[<RequireQualifiedAccess>]
module Dap.Platform.Agent

open Dap.Prelude
open Dap.Platform.Internal

let private loop (agent : Agent<'args, 'model, 'msg, 'req, 'evt>)
                    (mailbox : MailboxProcessor<'msg>) : Async<unit> =
    let rec handle() =
        async {
            let! msg = mailbox.Receive()
            agent.Process msg |> agent.Deliver
            return! handle()
        }
    handle()

let private start (agent : Agent<'args, 'model, 'msg, 'req, 'evt>) : unit =
    let mailbox = MailboxProcessor.Start(loop agent)
    agent.SetDispatch mailbox.Post
    agent.Start() |> agent.Deliver

let getLogger (kind : string) (key : string) =
    Logging.getLogger <| sprintf "%s.%s" kind key

let create (kind : Kind) (key : Key)
            (spec : ActorSpec<'args, 'model, 'msg, 'req, 'evt>)
                : IAgent<'args, 'model, 'msg, 'req, 'evt> =
    let ident = Ident.Create noScope kind key
    let logger = getLogger kind key
    let agent = new Agent<'args, 'model, 'msg, 'req, 'evt> (spec, ident, logger)
    start agent
    agent :> IAgent<'args, 'model, 'msg, 'req, 'evt>

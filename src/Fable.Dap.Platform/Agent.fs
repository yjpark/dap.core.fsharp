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
                : IAgent<'model, 'req, 'evt> =
    let agent : Agent<'args, 'model, 'msg, 'req, 'evt> = {
        Spec = spec
        Ident' = Ident.Create noScope kind key
        Logger' = getLogger kind key
        Dispatch = None
        State' = None
    }
    start agent
    agent :> IAgent<'model, 'req, 'evt>
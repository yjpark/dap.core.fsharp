[<RequireQualifiedAccess>]
module Dap.Platform.Actor

open Dap.Prelude
open Dap.Platform.Internal

let private loop (actor : Actor<'args, 'model, 'msg, 'req, 'evt>)
                    (mailbox : MailboxProcessor<'msg>) : Async<unit> =
    let rec handle() =
        async {
            let! msg = mailbox.Receive()
            actor.Process msg |> actor.Deliver
            return! handle()
        }
    handle()

let private start (actor : Actor<'args, 'model, 'msg, 'req, 'evt>) : unit =
    let mailbox = MailboxProcessor.Start(loop actor)
    actor.SetDispatch mailbox.Post
    actor.Start() |> actor.Deliver

let getLogger (kind : string) (key : string) =
    Logging.getLogger <| sprintf "%s.%s" kind key

let create (kind : Kind) (key : Key) 
            (spec : ActorSpec<IActor, 'args, 'model, 'msg, 'req, 'evt>)
                : IActor<'model, 'req, 'evt> =
    let actor : Actor<'args, 'model, 'msg, 'req, 'evt> = {
        Spec = spec
        Ident' =
            {
                Scope = ""
                Kind = kind
                Key = key
            }
        Logger' = getLogger kind key
        Dispatch = None
        State' = None
    }
    start actor
    actor :> IActor<'model, 'req, 'evt>
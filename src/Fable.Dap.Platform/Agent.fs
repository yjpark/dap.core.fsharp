[<AutoOpen>]
[<RequireQualifiedAccess>]
module Dap.Platform.Agent

open Dap.Prelude

let private loop (agent : BaseAgent<'runner, 'args, 'model, 'msg, 'req, 'evt>)
                    (mailbox : MailboxProcessor<'msg>) : Async<unit> =
    let rec handle() =
        async {
            let! msg = mailbox.Receive()
            agent.Process' msg |> agent.Deliver'
            return! handle()
        }
    handle()

let private start (agent : BaseAgent<'runner, 'args, 'model, 'msg, 'req, 'evt>) : unit =
    let mailbox = MailboxProcessor.Start(loop agent)
    agent.SetDispatch' mailbox.Post
    agent.Start' () |> agent.Deliver'

let internal spawn<'runner, 'args, 'model, 'msg, 'req, 'evt
        when 'runner :> BaseAgent<'runner, 'args, 'model, 'msg, 'req, 'evt>
            and 'model : not struct and 'msg :> IMsg
            and 'req :> IReq and 'evt :> IEvt>
        (spec : ActorSpec<'runner, 'args, 'model, 'msg, 'req, 'evt>)
        (param : AgentParam)
            : 'runner =
    let agent = spec.Spawner param
    agent.Setup' spec
    agent |> start
    agent


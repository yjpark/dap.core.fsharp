[<AutoOpen>]
module Dap.Platform.MailboxPlatform

open Dap.Prelude

let private loop (runnable : IRunnable<'initer, 'runner, 'args, 'model, 'msg>)
                    (mailbox : MailboxProcessor<Parcel<'msg>>) : Async<unit> =
    let rec handle() =
        async {
            let! (time, msg) = mailbox.Receive()
            //logWarn runnable "MailboxPlatform" "loop" msg
            trackDeliverDuration runnable "MailboxPlatform" time msg
            runnable.Process msg |> runnable.Deliver
            return! handle()
        }
    handle()

let private start (runnable : IRunnable<'initer, 'runner, 'args, 'model, 'msg>) : unit =
    let mailbox = MailboxProcessor.Start(loop runnable)
    runnable.SetDispatch mailbox.Post
    runnable.Start() |> runnable.Deliver

type MailboxPlatform (logging) =
    inherit BasePlatform (logging)
    override this.Start runnable = start runnable
    interface IFallback
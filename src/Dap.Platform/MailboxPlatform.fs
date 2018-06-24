[<AutoOpen>]
module Dap.Platform.MailboxPlatform

let private loop (runnable : IRunnable<'initer, 'runner, 'args, 'model, 'msg>)
                    (mailbox : MailboxProcessor<Parcel<'msg>>) : Async<unit> =
    let rec handle() =
        async {
            let! (time, msg) = mailbox.Receive()
            //logWip runnable "LOOP:HANDLE" msg
            trackDeliverDuration runnable time msg
            runnable.Process msg |> runnable.Deliver
            return! handle()
        }
    handle()

let private start (runnable : IRunnable<'initer, 'runner, 'args, 'model, 'msg>) : unit =
    let mailbox = MailboxProcessor.Start(loop runnable)
    runnable.SetDispatch mailbox.Post
    runnable.Start() |> runnable.Deliver

type MailboxPlatform = MailboxPlatform
with
    interface IPlatform with
        member _this.Start runnable = start runnable
[<AutoOpen>]
module Dap.Platform.Replier

open System.Threading.Tasks
open Dap.Prelude

type IAsyncHandler<'req> =
    abstract HandleAsync<'res> : (Callback<'res> -> 'req) -> Task<'res>

type IAsyncPoster<'msg> =
    abstract PostAsync<'res> : (Callback<'res> -> 'msg) -> Task<'res>

and GetReplyTask<'res> = IMsg -> Callback<'res> -> GetTask<unit>
and GetReplyTask'<'runner, 'res> = IMsg -> Callback<'res> -> GetTask'<'runner, unit>

and OnReplyFailed<'res> = IMsg -> Callback<'res> -> IRunner -> exn -> unit
and OnReplyFailed'<'runner, 'res> = IMsg -> Callback<'res> -> 'runner -> exn -> unit

let private tplAckReply = LogEvent.Template2<IMsg, obj>(AckLogLevel, "[Ack] {Req} ~> {Res}")
let private tplNakReply = LogEvent.Template3<IMsg, string, obj>(LogLevelError, "[Nak] {Req} ~> {Err}: {Detail}")

let private tplAckCallback = LogEvent.Template3<float<ms>, IMsg, obj>(AckLogLevel, "[Ack] {Duration}<ms> {Req} ~> {Res}")
let private tplSlowAckCallback = LogEvent.Template3<float<ms>, IMsg, obj>(LogLevelWarning, "[Ack] {Duration}<ms> {Req} ~> {Res}")
let private tplNakCallback = LogEvent.Template4<float<ms>, IMsg, string, obj>(LogLevelError, "[Nak] {Duration}<ms> {Req} ~> {Err}: {Detail}")

let private tplSlowStats = LogEvent.Template4<string, float<ms>, IMsg, DurationStats<ms>>(LogLevelWarning, "[{Section}] {Duration}<ms> {Msg} ~> {Detail}")

let ack (req : 'req) (res : 'res) =
    Ack (req, res)

let nak (req : 'req) (err : string) (detail : obj) =
    Nak (req, err, detail)

let reply (runner : IRunner) (callback : Callback<'res>) (reply : Reply<'res>) : unit = 
    match callback with
    | Some callback ->
        callback reply
    | None ->
        match reply with
        | Ack (req, res) ->
            //TODO: check log level in case should not create the message
            runner.Log <| tplAckReply req res
        | Nak (req, err, detail) ->
            runner.Log <| tplNakReply req err detail

let private getSlowReplyMessage req (duration, stats) =
    tplSlowStats "Slow_Reply" duration req stats

let callback' (runner : IRunner) onNak onAck : Callback<'res> =
    let sendTime = runner.Clock.Now'
    Some <| fun (r : Reply<'res>) ->
        match r with
            | Ack (req, res) ->
                runner.Stats.Reply.IncSucceedCount ()
                let (_, _, durationInMs, isSlow) = trackDurationStatsInMs runner sendTime runner.Stats.Reply.Duration (getSlowReplyMessage req)
                let tpl = if isSlow then tplSlowAckCallback else tplAckCallback
                runner.Log <| tpl durationInMs req res
                onAck res
            | Nak (req, err, detail) ->
                runner.Stats.Reply.IncFailedCount ()
                let (_, _, durationInMs, _) = trackDurationStatsInMs runner sendTime runner.Stats.Reply.Duration (getSlowReplyMessage req)
                let message = tplNakCallback durationInMs req err detail
                runner.Log <| message
                onNak (err, detail)

let callback (runner : IRunner) onAck : Callback<'res> =
    callback' runner (fun (e, d) -> ()) onAck

let callbackAsync (runner : IRunner) (onDone : TaskCompletionSource<'res>) : Callback<'res> =
    callback' runner
        (ReplyException >> onDone.SetException)
        onDone.SetResult

let nakOnFailed (msg : IMsg) (callback : Callback<'res>) (runner : IRunner) (e : exn) =
    reply runner callback <| nak msg "Exception_Raised" e

let nakOnFailed' (msg : IMsg) (callback : Callback<'res>) (runner : IRunner<'runner>) (e : exn) =
    reply runner callback <| nak msg "Exception_Raised" e


(* Note: for similar reason to the AwaitTask logic, this won't work, can compile
 * properly, though in the run time, if use this to reply, seems async logic in the func
 * will stuck forever. Leave the codes here in case want to try more in the future

let replyFunc (runner : IRunner) (msg : IMsg) (callback : Callback<'res>)
                (getOnFailed: OnReplyFailed<'res>)
                (func : Func<'res>) : unit =
    match runner.RunFunc func with
    | Ok _res ->
        ()
    | Error e ->
        getOnFailed msg callback runner e

let replyFunc' (runner : IRunner<'runner>) (msg : IMsg) (callback : Callback<'res>)
                (getOnFailed: OnReplyFailed'<'runner, 'res>)
                (func : Func'<'runner, 'res>) : unit =
    match runner.RunFunc' func with
    | Ok res ->
        reply runner callback <| ack msg res
    | Error e ->
        getOnFailed msg callback runner e
*)

let replyAsync (runner : IRunner) (msg : IMsg) (callback : Callback<'res>)
                (getOnFailed: OnReplyFailed<'res>)
                (getReplyTask : GetReplyTask<'res>) : unit =
    let getTask = getReplyTask msg callback
    let onFailed = getOnFailed msg callback
    runner.RunTask onFailed getTask

let replyAsync' (runner : IRunner<'runner>) (msg : IMsg) (callback : Callback<'res>)
                (getOnFailed: OnReplyFailed'<'runner, 'res>)
                (getReplyTask : GetReplyTask'<'runner, 'res>) : unit =
    let getTask = getReplyTask msg callback
    let onFailed = getOnFailed msg callback
    runner.RunTask' onFailed getTask

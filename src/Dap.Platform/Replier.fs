[<AutoOpen>]
module Dap.Platform.Replier

open System.Threading.Tasks
open FSharp.Control.Tasks.V2

open Dap.Prelude
open Dap.Context


let private tplAckReply = LogEvent.Template2<IReq, obj>(AckLogLevel, "[Ack] {Req} ~> {Res}")
let private tplNakReply = LogEvent.Template3<IReq, string, obj>(LogLevelError, "[Nak] {Req} ~> {Err}: {Detail}")

let private tplAckCallback = LogEvent.Template3<float<ms>, IReq, obj>(AckLogLevel, "[Ack] {Duration}<ms> {Req} ~> {Res}")
let private tplSlowAckCallback = LogEvent.Template3<float<ms>, IReq, obj>(LogLevelWarning, "[Ack] {Duration}<ms> {Req} ~> {Res}")
let private tplNakCallback = LogEvent.Template4<float<ms>, IReq, string, obj>(LogLevelError, "[Nak] {Duration}<ms> {Req} ~> {Err}: {Detail}")

let private tplSlowStats = LogEvent.Template4<string, float<ms>, IReq, DurationStats<ms>>(LogLevelWarning, "[{Section}] {Duration}<ms> {Msg} ~> {Detail}")

let ack (req : IReq) (res : 'res) =
    Ack (req, res)

let nak (req : IReq) (err : string) (detail : obj) =
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

let replyAfter (runner : IRunner) (callback : Callback<'res>) (reply' : Reply<'res>) : unit =
    runner.AddTask0 ignoreOnFailed <| fun _runner -> task {
        reply runner callback reply'
    }

let private getSlowReplyMessage req (duration, stats) =
    tplSlowStats "Slow_Reply" duration req stats

let callback' (runner : 'runner when 'runner :> IRunner) onNak onAck : Callback<'res> =
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

let nakOnFailed (req : IReq) (callback : Callback<'res>) (runner : 'runner) (e : exn) =
    reply runner callback <| nak req "Exception_Raised" e


//Note: the AddTask here is mainly for timing, to make sure
//the tasks are executed after the process, so the updateModel actions
//always took effect

let replyAsync (runner : 'runner when 'runner :> IRunner<'runner>) (req : IReq) (callback : Callback<'res>)
                (getOnFailed: OnReplyFailed<'runner, 'res>)
                (getReplyTask : GetReplyTask<'runner, 'res>) : unit =
    let onFailed = fun _ -> getOnFailed req callback runner.Runner
    let getTask = fun _ -> getReplyTask req callback runner.Runner
    runner.AddTask onFailed getTask

let replyAsync0 (runner : IRunner) (req : IReq) (callback : Callback<'res>)
                (getOnFailed: OnReplyFailed<IRunner, 'res>)
                (getReplyTask : GetReplyTask<IRunner, 'res>) : unit =
    let onFailed = getOnFailed req callback
    let getTask = getReplyTask req callback
    runner.AddTask0 onFailed getTask

let replyAsync1 (runner : IAgent) (req : IReq) (callback : Callback<'res>)
                (getOnFailed: OnReplyFailed<IAgent, 'res>)
                (getReplyTask : GetReplyTask<IAgent, 'res>) : unit =
    let onFailed = getOnFailed req callback
    let getTask = getReplyTask req callback
    runner.AddTask1 onFailed getTask

let replyAsync2 (runner : IAgent<'req, 'evt>) (req : IReq) (callback : Callback<'res>)
                (getOnFailed: OnReplyFailed<IAgent<'req, 'evt>, 'res>)
                (getReplyTask : GetReplyTask<IAgent<'req, 'evt>, 'res>) : unit =
    let onFailed = getOnFailed req callback
    let getTask = getReplyTask req callback
    runner.AddTask2 onFailed getTask

let replyAsync3 (runner : IAgent<'args, 'model, 'msg, 'req, 'evt>) (req : IReq) (callback : Callback<'res>)
                (getOnFailed: OnReplyFailed<IAgent<'args, 'model, 'msg, 'req, 'evt>, 'res>)
                (getReplyTask : GetReplyTask<IAgent<'args, 'model, 'msg, 'req, 'evt>, 'res>) : unit =
    let onFailed = getOnFailed req callback
    let getTask = getReplyTask req callback
    runner.AddTask3 onFailed getTask
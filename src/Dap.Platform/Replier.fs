[<AutoOpen>]
module Dap.Platform.Replier

open System.Threading.Tasks

open Dap.Prelude
open Dap.Context

let private tplAckReply = LogEvent.Template3<string, IReq, obj>(AckLogLevel, "[{Section}] {Req} ~> {Res}") "Ack"
let private tplNakReply = LogEvent.Template4<string, IReq, string, obj>(LogLevelError, "[{Section}] {Req} ~> {Err} : {Detail}") "Nak"

let private tplAckCallback = LogEvent.Template4<string, Duration, IReq, obj>(AckLogLevel, "[{Section}] {Duration}<ms> {Req} ~> {Res}") "Ack"
let private tplSlowAckCallback = LogEvent.Template4<string, Duration, IReq, obj>(LogLevelInformation, "[{Section}] {Duration} {Req} ~> {Res}") "Ack"
let private tplNakCallback = LogEvent.Template5<string, Duration, IReq, string, obj>(LogLevelError, "[{Section}] {Duration} {Req} ~> {Err} : {Detail}") "Nak"

let private tplSlowStats = LogEvent.Template5<string, Duration, IReq, string, string>(LogLevelInformation, "[{Section}] {Duration} {Msg} ~> {Detail}\n{StackTrace}")

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

type FuncStats with
    member this.AddReply (runner : 'runner when 'runner :> IRunner) (msg : string) (startTime : Instant) (reply : Reply<'res>) =
        let (duration, slowOp) =
            DurationStats.AddOp' this.Spec.Key this.SlowCap this.TotalCount this.SlowCount (fun () ->
                (System.Diagnostics.StackTrace(2)) .ToString()
            ) runner msg startTime
        let failedOp =
            match reply with
            | Ack (_req, _res) ->
                this.AddSucceedOp ()
            | Nak (_req, _err, _detail) ->
                let stackTrace = (System.Diagnostics.StackTrace(2)) .ToString()
                this.AddFailedOp msg startTime duration stackTrace
        (duration, slowOp, failedOp)

let callback' (runner : 'runner when 'runner :> IRunner) onNak onAck : Callback<'res> =
    let stats = runner.Dash0.Stats.Reply
    let sendTime = runner.Clock.Now'
    Some <| fun (r : Reply<'res>) ->
        let req =
            match r with
            | Ack (req, _res) -> req
            | Nak (req, _err, _detail) -> req
        let (duration, slowOp, _failedOp) = stats.AddReply runner (req.GetType().Name) sendTime r
        slowOp
        |> Option.iter (fun opLog ->
            runner.Log <| tplSlowStats "Slow_Reply" duration req  (stats.ToLogDetail ()) opLog.StackTrace
        )
        match r with
            | Ack (req, res) ->
                if slowOp.IsSome then
                    runner.Log <| tplSlowAckCallback duration req res
                else
                    runner.Log <| tplAckCallback duration req res
                onAck res
            | Nak (req, err, detail) ->
                runner.Log <| tplNakCallback duration req err detail
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
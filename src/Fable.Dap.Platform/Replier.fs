[<AutoOpen>]
module Dap.Platform.Replier

open Dap.Prelude
open Dap.Platform

let private tplAckReply = LogEvent.Template2<IMsg, obj>(AckLogLevel, "[Ack] {Req} ~> {Res}")
let private tplNakReply = LogEvent.Template3<IMsg, string, obj>(LogLevelError, "[Nak] {Req} ~> {Err}: {Detail}")
let private tplAckCallback = LogEvent.Template3<int, IMsg, obj>(AckLogLevel, "[Ack] {Duration}<ms> {Req} ~> {Res}")
let private tplSlowAckCallback = LogEvent.Template3<int, IMsg, obj>(LogLevelWarning, "[Ack] {Duration}<ms> {Req} ~> {Res}")
let private tplNakCallback = LogEvent.Template4<int, obj, string, obj>(LogLevelError, "[Nak] {Duration}<ms> {Req} ~> {Err}: {Detail}")

let ack<'req, 'res> (req : 'req) (res : 'res) : Response<'req, 'res> =
    Ack (req, res)

let nak<'req, 'res> (req : 'req) (err : string) (detail : obj) : Response<'req, 'res> =
    Nak (req, err, detail)

let reply<'res> (logger : ILogger) (callback : Callback<'res>) (reply : Reply<'res>) : unit = 
    match callback with
    | Some callback ->
        callback reply
    | None ->
        match reply with
        | Ack (req, res) ->
            //TODO: check log level in case should not create the message
            logger.Log <| tplAckReply req res
        | Nak (req, err, detail) ->
            logger.Log <| tplNakReply req err detail

let callback'<'res> (logger : ILogger) onAck onNak : Callback<'res> =
    let startTime = System.DateTime.UtcNow
    Some <| fun (r : Reply<'res>) ->
        let duration = System.DateTime.UtcNow - startTime
        let durationInMs = duration.Milliseconds
        match r with
            | Ack (req, res) ->
                let isSlow = durationInMs > ReplySlowCap
                let tpl = if isSlow then tplSlowAckCallback else tplAckCallback
                logger.Log <| tpl durationInMs req res
                onAck res
            | Nak (req, err, detail) ->
                logger.Log <| tplNakCallback durationInMs req err detail
                onNak (err, detail)

let callback<'res> (logger : ILogger) onAck : Callback<'res> =
    callback' logger onAck ignore

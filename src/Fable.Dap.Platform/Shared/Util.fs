[<AutoOpen>]
module Dap.Platform.Util

open Dap.Prelude
open Dap.Context

let private tplMsgInfo = LogEvent.Template4<string, IMsg, string, obj>(LogLevelInformation, "[{Section}] {Msg} ~> {Info}: {Detail}")

let private tplMsgError = LogEvent.Template4<string, IMsg, string, obj>(LogLevelError, "[{Section}] {Msg} ~> {Err}: {Detail}")

let private tplReqInfo = LogEvent.Template4<string, IReq, string, obj>(LogLevelInformation, "[{Section}] {Req} ~> {Info}: {Detail}")

let private tplReqError = LogEvent.Template4<string, IReq, string, obj>(LogLevelError, "[{Section}] {Req} ~> {Err}: {Detail}")

let private tplEvtInfo = LogEvent.Template4<string, IEvt, string, obj>(LogLevelInformation, "[{Section}] {Evt} ~> {Info}: {Detail}")

let private tplEvtError = LogEvent.Template4<string, IEvt, string, obj>(LogLevelError, "[{Section}] {Evt} ~> {Err}: {Detail}")

let logMsgInfo (logger : ILogger) section (msg : IMsg) info detail : unit =
    logger.Log <| tplMsgInfo section msg info detail

let logMsgError (logger : ILogger) section (msg : IMsg) err detail : unit =
    logger.Log <| tplMsgError section msg err detail

let logReqInfo (logger : ILogger) section (msg : IReq) info detail : unit =
    logger.Log <| tplReqInfo section msg info detail

let logReqError (logger : ILogger) section (msg : IReq) err detail : unit =
    logger.Log <| tplReqError section msg err detail

let logEvtInfo (logger : ILogger) section (msg : IEvt) info detail : unit =
    logger.Log <| tplEvtInfo section msg info detail

let logEvtError (logger : ILogger) section (msg : IEvt) err detail : unit =
    logger.Log <| tplEvtError section msg err detail

let updateModel (update : 'model -> 'model) : Operate<'runner, 'model, 'msg> =
    fun _runner (model, cmd) ->
        (update model, cmd)

#if !FABLE_COMPILER
let inline updateSession (update : 'session -> 'session) : Operate<'runner, ^model, 'msg> =
    fun _runner (model, cmd) ->
        let session = (^model : (member Session : 'session option) model)
        match session with
        | None ->
            (model, cmd)
        | Some session ->
            let newSession = update session
            let newModel = (^model : (member WithSession : 'session -> ^model) (model, newSession))
            (newModel, cmd)

let inline updateExtra (update : 'extra -> 'extra) : Operate<'runner, ^model, 'msg> =
    fun _runner (model, cmd) ->
        let extra = (^model : (member Extra : 'extra) model)
        let newExtra = update extra
        let newModel = (^model : (member WithExtra : 'extra -> ^model) (model, newExtra))
        (newModel, cmd)

#endif
let addCmd' (newCmd : Cmd<'msg>) : Operate<'runner, 'model, 'msg> =
    fun _runner (model, cmd) ->
        (model, batchCmd [cmd; newCmd])

let addCmd (msg : 'msg) : Operate<'runner, 'model, 'msg> =
    fun _runner (model, cmd) ->
        (model, batchCmd [cmd; cmdOfMsg msg])

let addSubCmd' (wrapper : Wrapper<'msg, 'subMsg>) (subCmd : Cmd<'subMsg>) : Operate<'runner, 'model, 'msg> =
    fun _runner (model, cmd) ->
        (model, batchCmd [cmd; mapCmd wrapper subCmd])

let addSubCmd (wrapper : Wrapper<'msg, 'subMsg>) (subMsg : 'subMsg) : Operate<'runner, 'model, 'msg> =
    wrapper subMsg
    |> addCmd

let noSubscription : Subscribe<'runner, 'model, 'msg> =
    fun _runner _model ->
        noCmd

let noOperation : Operate<'runner, 'model, 'msg> =
    fun _runner ->
        id

let noReaction : React<'runner, 'model, 'msg, 'subModel, 'subMsg> =
    fun _runner _subMsg _subModel model ->
        (model, [])

let noEvent =
    let bus = new Bus<NoEvt>(noOwner, "NoEvt")
    bus.Publish

let noCastEvt = fun _ -> None

// Note: Use this form to force the caller to provide proper type
// of 'model and 'msg, otherwise will get error of
// FS0030: Value restriction
let subscribeBus (owner : IOwner) (_model : 'model)
                    (wrapper : 'evt -> 'msg)
                    (onEvent : IBus<'evt>) : Cmd<'msg> =
    let sub = fun dispatch ->
        let ident = sprintf "%A" wrapper
        onEvent.AddWatcher owner ident (dispatch << wrapper)
    cmdOfSub sub

let subscribeEvent (owner : IOwner) (_model : 'model)
                    (wrapper : 'evt -> 'msg)
                    (onEvent : IEvent<'evt>) : Cmd<'msg> =
    let sub = fun dispatch ->
        let ident = sprintf "%A" wrapper
        onEvent.Add (dispatch << wrapper)
    cmdOfSub sub

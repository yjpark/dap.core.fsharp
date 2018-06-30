[<AutoOpen>]
module Dap.Platform.Util

open Elmish
#if FABLE_COMPILER
open Fable.Core
open Fable.PowerPack
#endif

open Dap.Prelude

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

let setModel (newModel : 'model) : Operate<'runner, 'model, 'msg> =
    fun _runner (_model, cmd) ->
        (newModel, cmd)

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

let inline setSession (newSession : 'session) : Operate<'runner, ^model, 'msg> =
    fun _runner (model, cmd) ->
        let newModel = (^model : (member WithSession : 'session -> ^model) (model, newSession))
        (newModel, cmd)
#endif
let noCmd = Cmd.none

let addCmd' (newCmd : Cmd<'msg>) : Operate<'runner, 'model, 'msg> =
    fun _runner (model, cmd) ->
        (model, Cmd.batch [cmd; newCmd])

let addCmd (msg : 'msg) : Operate<'runner, 'model, 'msg> =
    fun _runner (model, cmd) ->
        (model, Cmd.batch [cmd; Cmd.ofMsg msg])

let addSubCmd' (wrapper : Wrapper<'msg, 'subMsg>) (subCmd : Cmd<'subMsg>) : Operate<'runner, 'model, 'msg> =
    fun _runner (model, cmd) ->
        (model, Cmd.batch [cmd; Cmd.map wrapper subCmd])

let addSubCmd (wrapper : Wrapper<'msg, 'subMsg>) (subMsg : 'subMsg) : Operate<'runner, 'model, 'msg> =
    wrapper subMsg
    |> addCmd

let noSubscription : Subscribe<'runner, 'model, 'msg> =
    fun _runner _model ->
        Cmd.none

let noOperation : Operate<'runner, 'model, 'msg> =
    fun _runner ->
        id

let noLogic : Logic<'initer, 'runner, NoArgs, NoModel, NoMsg> =
    {
        Init = fun _initer _args -> (NoModel, Cmd.none)
        Update = fun _runner model _msg -> (model, Cmd.none)
        Subscribe = noSubscription
    }

let noReaction : React<'runner, 'model, 'msg, 'subModel, 'subMsg> =
    fun _runner _subMsg _subModel model ->
        (model, [])

let noEvent =
    let noOwner =
        let logger = getLogger "<noOwner>"
        { new IOwner with
            member _this.Log m = logger.Log m
            member _this.Ident = noIdent.Ident
            member _this.Disposed = false
        }
    let bus = new Bus<NoEvt>(noOwner)
    bus.Publish

let noActor<'initer, 'runner> : ActorSpec'<'initer, 'runner, NoArgs, NoModel, NoMsg, NoReq, NoEvt> =
    {
        NewArgs = fun _owner -> NoArgs
        Logic = noLogic
        WrapReq = fun _ -> NoMsg
        GetOnEvent = fun _model -> noEvent
    }

let noVersion =
    {
        StateVer = 0
        MsgCount = -1
    }

// Note: Use this form to force the caller to provide proper type
// of 'model and 'msg, otherwise will get error of 
// FS0030: Value restriction
let subscribeEvent (owner : IOwner) (_model : 'model)
                    (wrapper : 'evt -> 'msg)
                    (onEvent : IBus<'evt>) : Cmd<'msg> =
    let sub = fun dispatch ->
        let ident = sprintf "%A" wrapper
        onEvent.AddWatcher owner ident (dispatch << wrapper)
    Elmish.Cmd.ofSub sub

#if FABLE_COMPILER
type Second = float

let private sleepForSeconds (delay : Second) = promise {
    do! Promise.sleep <| int (delay * 1000.0)
    return delay
}

let addFutureCmd (delay : Second) (msg : 'msg) : Operate<'runner, 'model, 'msg> =
    Cmd.ofPromise
        sleepForSeconds delay
        (fun _ -> msg)
        (fun e ->
            Fable.Import.Browser.console.error ("addFutureCmd Failed:", [|box delay ; box msg |])
            Fable.Import.Browser.console.error ("Exception:", [|box e.Message ; box "\nStackTrace:" ; box e.StackTrace|])
            msg
        )
    |> addCmd'
#endif
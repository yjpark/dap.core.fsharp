[<AutoOpen>]
module Dap.Platform.Dispatcher

open System
open Dap.Prelude

type Parcel<'msg> when 'msg :> IMsg = Instant * 'msg

type DispatchMsg<'msg> when 'msg :> IMsg = Elmish.Dispatch<Parcel<'msg>>

type MessageDetail = Type * IMsg

type DispatcherDetail =
    System.Type * //Type
    System.Type * //MsgType
    bool //Has Dispatch

let private tplDispatchError = LogEvent.Template3<MessageDetail, obj, DispatcherDetail>(LogLevelError, "[Dispatch] {Msg} ~> {Err}: {Dispatcher}")
let private tplInvalidMsgError = LogEvent.Template4<Type, MessageDetail, obj, DispatcherDetail>(LogLevelError, "[Post] <{ExpectType}> {Msg} ~> {Err}: {Dispatcher}")

type IDispatcher<'msg> when 'msg :> IMsg =
    inherit IRunner
    abstract Dispatch : DispatchMsg<'msg> option with get
    abstract SetDispatch : DispatchMsg<'msg> -> unit

let private toDispatcherDetail (dispatcher : IDispatcher<'msg>) : DispatcherDetail =
    let hasDispatch =
        match dispatcher.Dispatch with
        | Some _ -> true
        | None -> false
    (dispatcher.GetType(), typeof<'msg>, hasDispatch)

let private toMessageDetail (msg : IMsg) : MessageDetail =
    (msg.GetType(), msg)

let internal dispatch' (dispatcher : IDispatcher<'msg>)
                (msg : 'msg when 'msg :> IMsg) : unit =
    match dispatcher.Dispatch with
    | Some dispatch ->
        dispatcher.Dash0.Stats.Reply.IncPendingCount ()
        dispatch (dispatcher.Clock.Now', msg)
    | None ->
        let msg' = toMessageDetail msg
        dispatcher.Log <| tplDispatchError msg' "Dispatch_Not_Set" (toDispatcherDetail dispatcher)

let internal invalidMsg' (dispatcher : IDispatcher<'msg>)
                (expect : System.Type)
                (msg : IMsg) : unit =
    let msg' = toMessageDetail msg
    dispatcher.Log <| tplInvalidMsgError expect msg' "Invalid_Msg" (toDispatcherDetail dispatcher)

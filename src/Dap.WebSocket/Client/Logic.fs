[<RequireQualifiedAccess>]
module Dap.WebSocket.Client.Logic

open System
open System.Threading.Tasks
open System.Net.WebSockets
open Dap.Prelude
open Dap.Platform
open Dap.WebSocket
open Dap.WebSocket.Types
open Dap.WebSocket.Internal.Tasks
open Dap.WebSocket.Client.Types
open Dap.WebSocket.Client.Tasks
module BaseLogic = Dap.WebSocket.Internal.Logic
module BaseTypes = Dap.WebSocket.Types

type ActorOperate<'pkt> = ActorOperate<ClientWebSocket, 'pkt, Req<'pkt>>

let private doConnect req (uri, token, callback) : ActorOperate<'pkt> =
    fun runner (model, cmd) ->
        match model.Status with
        | LinkStatus.Linked ->
            reply runner callback <| nak req "Already_Linked" model.Link
            (model, cmd)
        | _ ->
            let link : Link<ClientWebSocket> = {
                Ident = uri
                Token = token
                Socket = new ClientWebSocket()
                Buffer = Array.create<byte> runner.Actor.Args.BufferSize 0uy
            }
            replyAsync runner req callback doConnectFailed <| doConnectAsync
            (runner, model, cmd)
            |-|> updateModel (fun m -> {m with Link = Some link})
            |=|> addSubCmd InternalEvt ^<| DoRefreshStatus None

let private doDisconnect req (callback : Callback<unit>) : ActorOperate<'pkt> =
    fun runner (model, cmd) ->
        match model.Link with
        | Some link ->
            (runner, model, cmd)
            |=|> addSubCmd InternalEvt TryCloseSocket
        | None ->
            reply runner callback <| nak req "Link_Not_Exist" ()
            (model, cmd)

let private doSend req ((pkt, callback) : 'pkt * Callback<unit>) : ActorOperate<'pkt> =
    fun runner (model, cmd) ->
        BaseLogic.doSend runner req (pkt, callback)
        (model, cmd)

let handleReq req : ActorOperate<'pkt> =
    fun runner (model, cmd) ->
        match req with
        | DoConnect (a, b, c) -> doConnect req (a, b, c)
        | DoDisconnect a -> doDisconnect req a
        | DoSend (a, b) -> doSend req (a, b)
        <| runner <| (model, cmd)
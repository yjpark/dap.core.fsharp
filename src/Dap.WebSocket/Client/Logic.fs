[<RequireQualifiedAccess>]
module Dap.WebSocket.Client.Logic

open System
open System.Threading.Tasks
open System.Net.WebSockets
open FSharp.Control.Tasks
open Elmish
open Dap.Prelude
open Dap.Platform
open Dap.WebSocket
open Dap.WebSocket.Internal.Tasks
open Dap.WebSocket.Client.Types
open Dap.WebSocket.Client.Tasks
module BaseLogic = Dap.WebSocket.Internal.Logic
module BaseTypes = Dap.WebSocket.Types

type ActorOperate<'pkt> = ActorOperate<ClientWebSocket, 'pkt, Req<'pkt>>

let private doConnect msg (uri, token, callback) : ActorOperate<'pkt> =
    fun runner (model, cmd) ->
        match model.Link with
        | Some link ->
            reply runner callback <| nak msg "Link_Exist" link
            noOperation
        | None ->
            let link : Link<ClientWebSocket> = {
                Ident = uri
                Token = token
                Socket = new ClientWebSocket()
                Buffer = Array.create<byte> runner.Actor.Args.BufferSize 0uy
            }
            replyAsync3 runner msg callback nakOnFailed <| doConnectAsync
            setModel {model with Link = Some link}
        <| runner <| (model, cmd)

let private doSend msg ((pkt, callback) : 'pkt * Callback<SendStats>) : ActorOperate<'pkt> =
    fun runner (model, cmd) ->
        BaseLogic.doSend runner msg (pkt, callback)
        (model, cmd)

let private handleReq msg req : ActorOperate<'pkt> =
    fun runner (model, cmd) ->
        match req with
        | DoConnect (a, b, c) -> doConnect msg (a, b, c)
        | DoSend (a, b) -> doSend msg (a, b)
        <| runner <| (model, cmd)

let getSpec (encode : Encode<'pkt>) (decode : Decode<'pkt>) (logTraffic : bool) (bufferSize : int option) =
    fun owner ->
        {
            LogTraffic = logTraffic
            SendType = WebSocketMessageType.Text
            BufferSize = defaultArg bufferSize DefaultBufferSize
            Encode = encode
            Decode = decode
            Event' = new Bus<Evt<'pkt>> (owner)
            HandleReq = handleReq
        }
    |> BaseLogic.getSpec
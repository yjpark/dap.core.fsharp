[<RequireQualifiedAccess>]
module Dap.WebSocket.Conn.Logic

open System
open System.Threading.Tasks
open System.Net.WebSockets
open FSharp.Control.Tasks
open Elmish
open Dap.Prelude
open Dap.Platform
open Dap.WebSocket
open Dap.WebSocket.Internal.Tasks
open Dap.WebSocket.Conn.Types
module BaseLogic = Dap.WebSocket.Internal.Logic

type ActorOperate<'pkt> = ActorOperate<WebSocket, 'pkt, Req<'pkt>>
let private doConnect req (ident, token, socket, callback) : ActorOperate<'pkt> =
    fun runner (model, cmd) ->
        match model.Link with
        | Some link ->
            reply runner callback <| nak req "Link_Exist" link
            (model, cmd)
        | None ->
            let link : Link<WebSocket> = {
                Ident = ident
                Token = token
                Socket = socket
                Buffer = Array.create<byte> runner.Actor.Args.BufferSize 0uy
            }
            let task = doReceiveAsync runner
            reply runner callback <| ack req (task :> Task)
            (runner, model, cmd)
            |-|> setModel {model with Link = Some link}
            |=|> addCmd ^<| WebSocketEvt OnConnected

let private doSend req ((pkt, callback) : 'pkt * Callback<SendStats>) : ActorOperate<'pkt> =
    fun runner (model, cmd) ->
        BaseLogic.doSend runner req (pkt, callback)
        (model, cmd)

let private handleReq req : ActorOperate<'pkt> =
    fun runner (model, cmd) ->
        match req with
        | DoConnect (a, b, c, d) -> doConnect req (a, b, c, d)
        | DoSend (a, b) -> doSend req (a, b)
        <| runner <| (model, cmd)

let getSpec (encode : Encode<'pkt>) (decode : Decode<'pkt>) (logTraffic : bool) (bufferSize : int option) =
    fun _agent ->
        {
            LogTraffic = logTraffic
            SendType = WebSocketMessageType.Text
            BufferSize = defaultArg bufferSize DefaultBufferSize
            Encode = encode
            Decode = decode
            HandleReq = handleReq
        }
    |> BaseLogic.getSpec
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
let private doConnect msg (ident, token, socket, callback) : ActorOperate<'pkt> =
    fun runner (model, cmd) ->
        match model.Link with
        | Some link ->
            reply runner callback <| nak msg "Link_Exist" link
            noOperation
        | None ->
            let link : Link<WebSocket> = {
                Ident = ident
                Token = token
                Socket = socket
                Buffer = Array.create<byte> runner.Actor.Args.BufferSize 0uy
            }
            let task = doReceiveAsync runner
            reply runner callback <| ack msg (task :> Task)
            runner.Actor.Args.FireEvent' OnConnected
            setModel {model with Link = Some link}
        <| runner <| (model, cmd)

let private doSend msg ((pkt, callback) : 'pkt * Callback<SendStats>) : ActorOperate<'pkt> =
    fun runner (model, cmd) ->
        BaseLogic.doSend runner msg (pkt, callback)
        (model, cmd)

let private handleReq msg req : ActorOperate<'pkt> =
    fun runner (model, cmd) ->
        match req with
        | DoConnect (a, b, c, d) -> doConnect msg (a, b, c, d)
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
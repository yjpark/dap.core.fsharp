[<RequireQualifiedAccess>]
module Dap.WebSocket.Conn.Logic

open System
open System.Threading.Tasks
open System.Net.WebSockets
open FSharp.Control.Tasks.V2
open Elmish
open Dap.Prelude
open Dap.Platform
open Dap.WebSocket
open Dap.WebSocket.Internal.Tasks
open Dap.WebSocket.Conn.Types
module BaseLogic = Dap.WebSocket.Internal.Logic

type ActorOperate<'pkt> = ActorOperate<WebSocket, 'pkt, Req<'pkt>>

let private doAttach req (ident, token, socket, callback) : ActorOperate<'pkt> =
    fun runner (model, cmd) ->
        match model.Link with
        | Some link ->
            reply runner callback <| nak req "Link_Exist" link
            (model, cmd)
        | None ->
            let time = runner.Clock.Now
            let processTime = time
            let link : Link<WebSocket> = {
                Ident = ident
                Token = token
                Socket = socket
                Buffer = Array.create<byte> runner.Actor.Args.BufferSize 0uy
            }
            let task = doReceiveAsync runner
            reply runner callback <| ack req (task :> Task)
            let (time, duration) = runner.Clock.CalcDuration(time)
            let stats : LinkedStats = {
                ProcessTime = processTime
                ConnectedTime = time
                ConnectDuration = duration
            }
            (runner, model, cmd)
            |-|> updateModel (fun m -> {m with Link = Some link})
            |=|> addCmd ^<| InternalEvt ^<| OnLinked stats

let private doSend req ((pkt, callback) : 'pkt * Callback<SendStats>) : ActorOperate<'pkt> =
    fun runner (model, cmd) ->
        BaseLogic.doSend runner req (pkt, callback)
        (model, cmd)

let private handleReq req : ActorOperate<'pkt> =
    fun runner (model, cmd) ->
        match req with
        | DoAttach (a, b, c, d) -> doAttach req (a, b, c, d)
        | DoSend (a, b) -> doSend req (a, b)
        <| runner <| (model, cmd)

let spec sendType (encode : Encode<'pkt>) (decode : Decode<'pkt>) (logTraffic : bool) (bufferSize : int option) =
    {
        LogTraffic = logTraffic
        SendType = sendType
        BufferSize = defaultArg bufferSize DefaultBufferSize
        Encode = encode
        Decode = decode
        HandleReq = handleReq
    }|> BaseLogic.spec
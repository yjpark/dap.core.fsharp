module Dap.Archive.WebSocket.Accessor.Tasks

open FSharp.Control.Tasks.V2
open Dap.Prelude
open Dap.Platform
open Dap.Archive
open Dap.Archive.Storages
open Dap.Archive.Recorder
open Dap.Archive.WebSocket
open Dap.WebSocket
open Dap.Archive.WebSocket.Accessor.Types

module WebSocketTypes = Dap.WebSocket.Types
module WebSocketClientTypes = Dap.WebSocket.Client.Types

let private createRecorderAsync (client : Client<'pkt>) : GetTask<Part<'actorMsg, 'pkt>, EventRecorder.Agent option> =
    fun runner -> task {
        match runner.Part.Args.CreateRecorderAsync with
        | None -> return None
        | Some createRecorderAsync ->
            let! recorder = createRecorderAsync client
            return recorder
    }

let internal doSetupAsync : GetReplyTask<Part<'actorMsg, 'pkt>, unit> =
    fun req callback runner -> task {
        let uri = runner.Part.State.Uri |> Option.get
        let clientKey = sprintf "%s_%s" runner.Ident.Kind runner.Ident.Key
        let! (client, isNew) = runner.Env.HandleAsync <| DoGetAgent runner.Part.Args.ClientKind clientKey
        if not isNew then
            failWith "Client_Is_Not_New" (uri, client)
        let client = client :?> Client<'pkt>
        let! recorder = createRecorderAsync client runner
        runner.Deliver <| InternalEvt ^<| OnSetup (req, callback, client, recorder)
    }

let internal doConnectAsync : GetTask<Part<'actorMsg, 'pkt>, WebSocketTypes.LinkedStats> =
    fun runner -> task {
        let uri = runner.Part.State.Uri |> Option.get
        let cts = runner.Part.State.Cts
        let client = runner.Part.State.Client |> Option.get
        let! stats = client.PostAsync <| WebSocketClientTypes.DoConnect uri cts.Token
        return stats
    }

let internal doReconnectFailed : OnFailed<Part<'actorMsg, 'pkt>> =
    fun runner e ->
        if runner.Part.State.Status <> LinkStatus.Closed then
            runner.Deliver <| InternalEvt ^<| OnClosed None


let internal doReconnectAsync : GetTask<Part<'actorMsg, 'pkt>, unit> =
    fun runner -> task {
        if runner.Part.State.Status <> LinkStatus.Linking
                && runner.Part.State.Status <> LinkStatus.Linked then
            let! stats = doConnectAsync runner
            let! stats = doConnectAsync runner
            ()
        return ()
    }

let internal doStartAsync : GetReplyTask<Part<'actorMsg, 'pkt>, WebSocketTypes.LinkedStats option> =
    fun req callback runner -> task {
        let! stats = doConnectAsync runner
        reply runner callback <| ack req ^<| Some stats
    }

let internal doStopAsync : GetReplyTask<Part<'actorMsg, 'pkt>, unit> =
    fun req callback runner -> task {
        let client = runner.Part.State.Client |> Option.get
        do! client.PostAsync <| WebSocketClientTypes.DoDisconnect
        reply runner callback <| ack req ()
    }

let internal doSendAsync (pkt : 'pkt) : GetReplyTask<Part<'actorMsg, 'pkt>, WebSocketTypes.SendStats> =
    fun req callback runner -> task {
        let client = runner.Part.State.Client |> Option.get
        let! stats = client.PostAsync <| WebSocketClientTypes.DoSend pkt
        reply runner callback <| ack req stats
    }

let internal callOnLinkedAsync (onLinkedAsync : GetTask<Client<'pkt>, unit>) : GetTask<Part<'actorMsg, 'pkt>, unit> =
    fun runner -> task {
        let client = runner.Part.State.Client |> Option.get
        do! onLinkedAsync client
    }
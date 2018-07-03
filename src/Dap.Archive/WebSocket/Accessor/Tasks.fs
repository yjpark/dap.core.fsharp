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

let private createRecorderAsync (client : Client<'pkt>) : GetTask<Part<'pkt>, EventRecorder.Agent option> =
    fun runner -> task {
        match runner.Actor.Args.CreateRecorderAsync with
        | None -> return None
        | Some createRecorderAsync ->
            let! recorder = createRecorderAsync client
            return (Some recorder)
    }

let internal doSetupAsync : GetReplyTask<Part<'pkt>, unit> =
    fun req callback runner -> task {
        let uri = runner.Actor.State.Uri |> Option.get
        let clientKey = sprintf "%s_%s" runner.Ident.Kind runner.Ident.Key
        let! (client, isNew) = runner.Env.HandleAsync <| DoGetAgent' runner.Actor.Args.ClientKind clientKey
        if not isNew then
            raiseWithError "Accessor" "Client_Is_Not_New" (uri, client)
        let client = client :?> Client<'pkt>
        let! recorder = createRecorderAsync client runner
        runner.Deliver <| InternalEvt ^<| OnSetup (req, callback, client, recorder)
    }

let internal doStartAsync : GetReplyTask<Part<'pkt>, WebSocketTypes.ConnectedStats option> =
    fun req callback runner -> task {
        let uri = runner.Actor.State.Uri |> Option.get
        let cts = runner.Actor.State.Cts
        let client = runner.Actor.State.Client |> Option.get
        let! stats = client.PostAsync <| WebSocketClientTypes.DoConnect' uri cts.Token
        reply runner callback <| ack req ^<| Some stats
    }

let internal doStopAsync : GetReplyTask<Part<'pkt>, unit> =
    fun req callback runner -> task {
        let client = runner.Actor.State.Client |> Option.get
        do! client.PostAsync <| WebSocketClientTypes.DoDisconnect
        reply runner callback <| ack req ()
    }

let internal doSendAsync (pkt : 'pkt) : GetReplyTask<Part<'pkt>, WebSocketTypes.SendStats> =
    fun req callback runner -> task {
        let client = runner.Actor.State.Client |> Option.get
        let! stats = client.PostAsync <| WebSocketClientTypes.DoSend' pkt
        reply runner callback <| ack req stats
    }
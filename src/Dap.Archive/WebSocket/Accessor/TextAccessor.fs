[<RequireQualifiedAccess>]
module Dap.Archive.WebSocket.Accessor.TextAccessor

open Dap.Platform

module BaseTypes = Dap.Archive.WebSocket.Accessor.Types
module BaseLogic = Dap.Archive.WebSocket.Accessor.Logic

type Client = BaseTypes.Client<string>

type Part<'actorMsg> when 'actorMsg :> IMsg = BaseTypes.Part<'actorMsg, string>

type Args = BaseTypes.Args<string>
type Model = BaseTypes.Model<string>
type Msg = BaseTypes.Msg<string>
type Req = BaseTypes.Req<string>
type Evt = BaseTypes.Evt<string>

let create<'actorRunner, 'actorModel, 'actorMsg
            when 'actorRunner :> IAgent<'actorMsg> and 'actorMsg :> IMsg>
        (args : Args) partMsg wrapMsg agent =
    let spec = BaseLogic.spec<'actorMsg, string> args
    agent |> Part.create<'actorRunner, 'actorModel, 'actorMsg, Part<'actorMsg>, Args, Model, Msg, Req, Evt> spec partMsg wrapMsg

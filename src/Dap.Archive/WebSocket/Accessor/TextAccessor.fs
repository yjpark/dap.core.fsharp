[<RequireQualifiedAccess>]
module Dap.Archive.WebSocket.Accessor.TextAccessor

open Dap.Platform

module BaseTypes = Dap.Archive.WebSocket.Accessor.Types
module BaseLogic = Dap.Archive.WebSocket.Accessor.Logic

type Client = BaseTypes.Client<string>

type Part<'actorMsg> = BaseTypes.Part'<'actorMsg, string>

type Args = BaseTypes.Args<string>
type Model = BaseTypes.Model<string>
type Msg = BaseTypes.Msg<string>
type Req = BaseTypes.Req<string>
type Evt = BaseTypes.Evt<string>

let init modMsg wrapMsg getMod setMod agent (newArgs : PartNewArgs<Args>) =
    (BaseLogic.getSpec newArgs)
    |> Part.init modMsg wrapMsg getMod setMod agent

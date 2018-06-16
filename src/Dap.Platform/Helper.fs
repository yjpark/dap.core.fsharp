[<AutoOpen>]
module Dap.Platform.Helper

open Dap.Prelude

let DoQuit' (forceQuit : bool) callback =
    DoQuit (forceQuit, callback)

let DoAddService' (service : IAgent) callback =
    DoAddService (service, callback)

let DoGetService' (kind : Kind) callback =
    DoGetService (kind, callback)

let DoRegister' (kind : Kind) (spawner : Spawner) callback =
    DoRegister (kind, spawner, callback)

let DoGetAgent' (kind : Kind) (key : Key) callback =
    DoGetAgent (kind, key, callback)

let DoStop' (forceStop : bool) callback =
    DoStop (forceStop, callback)

let noAgent : AgentSpec<NoArgs, NoModel, NoMsg, NoReq, NoEvt> =
    {
        Actor = noActor
        OnAgentEvent = None
        GetSlowCap = None
    }

let calcSha256Sum (content : string) : string =
    use sha256 = System.Security.Cryptography.SHA256.Create()
    let hash = sha256.ComputeHash (System.Text.Encoding.UTF8.GetBytes content)
    System.Convert.ToBase64String hash

let calcSha256SumWithSalt (content : string) (salt : string) : string =
    calcSha256Sum <| content + salt

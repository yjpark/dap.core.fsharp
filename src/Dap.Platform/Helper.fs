[<AutoOpen>]
module Dap.Platform.Helper

open Dap.Prelude

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

[<AutoOpen>]
module Dap.Platform.Helper

open System.IO
open Dap.Prelude

let noAgent : AgentSpec<NoArgs, NoModel, NoMsg, NoReq, NoEvt> =
    {
        Actor = noActor
        OnAgentEvent = None
        GetSlowCap = None
    }

let addFutureCmd (delay : float<second>) (msg : 'msg) : ActorOperate<'args, 'model, 'msg, 'req, 'evt> =
    fun runner (model, cmd) ->
        let interval = 1000.0 * (float delay)
        let timer = new System.Timers.Timer(Interval = interval, Enabled = true, AutoReset = false)
        timer.Elapsed.AddHandler(new System.Timers.ElapsedEventHandler(fun _src _evt ->
            runner.Deliver <| msg
        ))
        (model, cmd)

let calcSha256Sum (content : string) : string =
    use sha256 = System.Security.Cryptography.SHA256.Create()
    let hash = sha256.ComputeHash (System.Text.Encoding.UTF8.GetBytes content)
    System.Convert.ToBase64String hash

let calcSha256SumWithSalt (content : string) (salt : string) : string =
    calcSha256Sum <| content + salt

let checkDirectory (runner : IRunner) (path : string) (section : string) =
    let dirInfo = (new FileInfo (path)).Directory;
    if not dirInfo.Exists then
        dirInfo.Create();
        logInfo runner section "Directory_Created" dirInfo

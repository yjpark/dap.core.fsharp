module Dap.Platform.Demo

open System.Threading
open System.Threading.Tasks
open FSharp.Control.Tasks.V2
open Dap.Prelude
open Dap.Platform
open Dap.WebSocket.Client

let doSimpleTestAsync (env : IEnv) delay : Task<unit> = task {
    let! _ = env.HandleAsync <| DoRegister "Dummy" ^<| Agent.getSpawner env noAgent
    let! (agent, isNew) = env.HandleAsync <| DoGetAgent "Dummy" "test"
    logInfo agent "Simple" (if isNew then "Created" else "Returned") agent.Ident

    logInfo env "Simple" "Delay_For_Seconds" delay
    do! Task.Delay (int ^<| delay * msPerSecond)
}

let doConnectAndSendTextAsync (env : IEnv) (delay : float<second>) : Task<unit> = task {
    let spawner = TextClient.getUtf8Spawner env true None
    let! _ = env.HandleAsync <| DoRegister TextClient.Kind spawner
    let! (agent, _) = env.HandleAsync <| DoGetAgent TextClient.Kind "test"
    let cts = new CancellationTokenSource()
    let agent = agent :?> TextClient.Agent
    agent.Actor.OnEvent.Add(logWip agent "OnEvent")
    let! _ = agent.PostAsync <| DoConnect "wss://echo.websocket.org" cts.Token
    let! _ = agent.PostAsync <| DoSend "Async_Test"
    logInfo env "WebSocket" "Delay_For_Seconds" delay
    do! Task.Delay (int ^<| delay * msPerSecond)
}

let rec onGetAgent ((agent, isNew) : IAgent * bool) =
    logInfo agent "onGetAgent" (if isNew then "Created" else "Returned") agent.Ident
    if isNew then
        agent.Env.Handle ^<| DoGetAgent ("Dummy", "test2", callback agent onGetAgent)

let doSimpleTest (env : IEnv) : unit =
    env |> Env.register "Dummy" noAgent |> ignore
    env.Handle <| DoGetAgent ("Dummy", "test", callback env onGetAgent)

[<EntryPoint>]
let main _argv =
    let logging = setupConsole None
    let env = Env.live MailboxPlatform logging "Demo"

    //doSimpleTest env
    //Async.AwaitTask.WaitTask <| doSimpleTestAsync env 0.1<second>
    //|> Async.RunSynchronously

    Async.AwaitTask <| doConnectAndSendTextAsync env 3.0<second>
    |> Async.RunSynchronously

    printfn "Quit..."
    0 // return an integer exit code


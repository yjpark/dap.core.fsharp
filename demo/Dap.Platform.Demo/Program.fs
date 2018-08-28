module Dap.Platform.Demo

open System.Threading
open System.Threading.Tasks
open FSharp.Control.Tasks.V2

open Dap.Prelude
open Dap.Context
open Dap.Context.Builder
open Dap.Context.Unsafe
open Dap.Platform
open Dap.WebSocket.Client

(*
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
*)

type Publisher (owner : IOwner, key : Key) =
    inherit WrapProperties<Publisher, IComboProperty> ()
    let target = Properties.combo owner key
    let name = target.AddString "name" "John Doe" None
    let year = target.AddInt "year" 1990 None
    do (
        target.SealCombo ()
        base.Setup (target)
    )
    static member Empty () = new Publisher (noOwner, NoKey)
    override this.Self = this
    override __.Spawn o k = new Publisher (o, k)
    override __.SyncTo t = target.SyncTo t.Target
    member __.Name = name
    member __.Year = year

type PublisherBuilder () =
    inherit ObjBuilder<Publisher> ()
    override __.Zero () =
        Publisher.Empty ()
    [<CustomOperation("name")>]
    member __.Name (this : Publisher, v) =
        this.Name.SetValue v |> ignore
        this
    [<CustomOperation("year")>]
    member __.Year (this : Publisher, v) =
        this.Year.SetValue v |> ignore
        this

let publisher = new PublisherBuilder ()

let doContextTest (env : IEnv) : unit =
    let author = combo {
        string "name" "John Doe" None
        int "age" 30 None
    }
    let book = context "Book" {
        properties (combo {
            bool "published" false None
            int "copies" 100 None
            combo "author" author
            custom "publisher1" (Publisher.Empty ())
            custom "publisher" (publisher {
                name "test"
                year 2001
            })
        })
    }
    logWarn book "Test" "Book.copied" (book.AsCombo.Properties.Get "copies")
    logWarn book "Test" "Init" (E.encodeJson 4 book)
    let copies =
        book.Properties0.AsCombo.Get "copies"
        :?> IVarProperty<int>
    copies.OnValueChanged.AddWatcher env "test" (fun evt ->
        logWarn book "Test" "copies.OnValueChanged" evt
    )
    copies.SetValue 0
    |> ignore
    logWarn book "Test" "Updated" (E.encodeJson 4 book)
    let pub = context "Publisher" {
        properties (publisher {
            name "a new publisher"
            year 2010
        })
    }
    let context = pub.ToCustom<Publisher> ()
    logWarn pub "Test" "Publisher_Context" context.Properties.Name.Value
    logWarn pub "Test" "Publisher_Context" (E.encodeJson 4 pub)

[<EntryPoint>]
let main _argv =
    let logging = setupConsole LogLevelWarning
    let env = Env.live MailboxPlatform logging "Demo"
    doContextTest env

    (*
    //doSimpleTest env
    //Async.AwaitTask.WaitTask <| doSimpleTestAsync env 0.1<second>
    //|> Async.RunSynchronously

    Async.AwaitTask <| doConnectAndSendTextAsync env 3.0<second>
    |> Async.RunSynchronously
    *)

    printfn "Quit..."
    0 // return an integer exit code


﻿module Demo.Program

open System.Threading
open System.Threading.Tasks

open Dap.Prelude
open Dap.Context
open Dap.Context.Unsafe
open Dap.Context.Builder
open Dap.Context.Generator
open Dap.Platform
open Dap.WebSocket.Client

open Demo.Builder

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

(*
open Dap.Platform.Demo.Builder
let doBuilderTest (env : IEnv) : unit =
    let person = combo {
        string "name" "John Doe"
        int "age" 30
    }
    let author = extend person {
        string "publisher" "No Publisher"
    }
    let book = context "Book" {
        properties (combo {
            bool "published" false
            int "copies" 100
            combo "author" author
            custom "publisher1" (PublisherProperty.Create ())
            custom "publisher" (publisher {
                name "test"
                year 2001
            })
        })
    }
    logWarn book "Test" "Book.copied" (book.AsCombo.Properties.Get "copies")
    logWarn book "Test" "Init" (encodeJson 4 book)
    let copies =
        book.Properties.AsCombo.Get "copies"
        :?> IVarProperty<int>
    copies.OnValueChanged.AddWatcher env "test" (fun evt ->
        logWarn book "Test" "copies.OnValueChanged" evt
    )
    copies.SetValue 0
    |> ignore
    logWarn book "Test" "Updated" (encodeJson 4 book)
    let pub = context "Publisher" {
        properties (publisher {
            name "a new publisher"
            year 2010
        })
    }
    let context = pub.ToCustom<PublisherProperty> ()
    logWarn pub "Test" "Publisher_Context" context.Properties.Name.Value
    logWarn pub "Test" "Publisher_Context" (encodeJson 4 pub)
 *)

    (*
let doJsonTest (env : IEnv) =
    let s = Demo.Types.Status.CreatePublished "Test" 2001 (Some 100)
    let s = encodeJson 0 s
    logWarn env "Test" "Encode_Status" s
    let s = decodeJson Demo.Types.Status.JsonDecoder s
    logWarn env "Test" "Decode_Status" s
    let s = encodeJson 0 s
    logWarn env "Test" "Decode_Status" s
    *)

let doCustomCloneTest (logger : ILogger) =
    let b =
        Dap.Context.Builder.Helper.combo {
            var (Property.string noOwner "test" "help" None)
        }
    let a =
        author {
            name "Test"
            age 20
            publisher "Some One"
            books b
        }
    logWarn logger "Test" "Original" <| encodeJson 4 a
    logWarn logger "Test" "Clone" <| encodeJson 4 ^<| a.AsProperty.Clone0 (noOwner, "Clone")

type App with
    static member CreateAsync (logging : ILogging, args : AppArgs) =
        let onSetup = new TaskCompletionSource<IApp>();
        let args =
            args
            |> AppArgs.SetScope "Demo"
            |> AppArgs.SetSetup onSetup.SetResult
        new App (logging, args) |> Feature.startApp
        onSetup.Task
    static member Create (logging : ILogging, args : AppArgs) =
        App.CreateAsync (logging, args)
        |> syncTask

[<EntryPoint>]
let main _argv =
    let logging = setupConsole LogLevelWarning

    (*
    let logger = getLogger "Prepare"
    Demo.Dsl.compile []
    |> List.iter (fun l -> logWarn logger "Dsl" l ())
    doCustomCloneTest (getLogger "Clone")
    *)

    (*
    let d = Duration.FromSeconds 1.0
    let json = E.encode 0 <| d.ToJson ()
    logWarn logger "Test" "Encode" (d, json)
    logWarn logger "Test" "Hack" ("{\"v\":" + json + "}")
    let printResult = fun result ->
        match result with
        | Ok d ->
            logWarn logger "Test" "Encode_Succeed" <| sprintf "%A" d
        | Error e ->
            logWarn logger "Test" "Encode_Failed" <| sprintf "%A" e
    printResult <| tryDecodeJsonString Duration.JsonDecoder json
    printResult <| tryDecodeJsonValue Duration.JsonDecoder json
    printResult <| tryDecodeJson Duration.JsonDecoder json
    *)
    let app = App.Create (logging, AppArgs.Create ())
    app.Ticker.Actor.OnEvent.AddWatcher app.Env "Test" (fun evt ->
        logWarn app.Env "Ticker" "OnEvent" evt
    )
    Task.Delay 1.0<second>
    |> Async.AwaitTask |> Async.RunSynchronously |> ignore
    let stats = app.Ticker.Console.GetStats.Handle ()
    logWarn app.Env "Ticker" "Stats" (E.encode 4 stats)
    app.Ticker.Console.Stats.AsProperty.LoadJson' stats |> ignore

    //doJsonTest env
    //doBuilderTest env

    (*
    //doSimpleTest env
    //Async.AwaitTask.WaitTask <| doSimpleTestAsync env 0.1<second>
    //|> Async.RunSynchronously

    Async.AwaitTask <| doConnectAndSendTextAsync env 3.0<second>
    |> Async.RunSynchronously
    *)

    //printfn "Quit..."
    0 // return an integer exit code


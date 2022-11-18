open Fake.Core
open Fake.IO.Globbing.Operators

open Dap.Build

let projects =
    !! "../src/Dap.Platform/*.fsproj"
    ++ "../src/Dap.WebSocket/*.fsproj"
    ++ "../src/Dap.Archive/*.fsproj"

let createTargets () =
    DotNet.create DotNet.debug projects;
    DotNet.createPrepares [
        ["Dap.Platform"], fun _ ->
            Dap.Platform.Dsl.Args.compile ["../src" ; "Dap.Platform"]
            |> List.iter traceSuccess
            Dap.Platform.Dsl.Dash.compile ["../src" ; "Dap.Platform"]
            |> List.iter traceSuccess
            Dap.Platform.Dsl.Packs.compile ["../src" ; "Dap.Platform"]
            |> List.iter traceSuccess
        ["Dap.WebSocket"], fun _ ->
            Dap.WebSocket.Dsl.compile ["../src" ; "Dap.WebSocket"]
            |> List.iter traceSuccess
        ["Dap.Archive"], fun _ ->
            Dap.WebSocket.Dsl.compile ["../src" ; "Dap.Archive"]
            |> List.iter traceSuccess
    ]

[<EntryPoint>]
let main argv =
    argv
    |> Array.toList
    |> Context.FakeExecutionContext.Create false "build.fsx"
    |> Context.RuntimeContext.Fake
    |> Context.setExecutionContext
    createTargets ()
    Target.runOrDefaultWithArguments DotNet.Prepare
    0
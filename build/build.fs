open Fake.Core
open Fake.IO.Globbing.Operators

open Dap.Build

let feed =
    NuGet.Feed.Create (
        apiKey = NuGet.Environment "API_KEY_nuget_org"
        //server = NuGet.ProGet "https://nuget.yjpark.org/nuget/dap",
        //apiKey = NuGet.Environment "API_KEY_nuget_yjpark_org"
    )

let projects =
    //!! "../lib/Dap.FlatBuffers/*.csproj"
    !! "../src/Fable.Dap.Prelude/*.fsproj"
    ++ "../src/Dap.Prelude/*.fsproj"
    ++ "../src/Fable.Dap.Context/*.fsproj"
    ++ "../src/Dap.Context/*.fsproj"
    ++ "../src/Fable.Dap.Platform/*.fsproj"
    ++ "../src/Dap.Platform.Cli/*.csproj"
    ++ "../src/Dap.Platform/*.fsproj"
    ++ "../src/Fable.Dap.WebSocket/*.fsproj"
    ++ "../src/Dap.WebSocket/*.fsproj"
    ++ "../src/Fable.Dap.Remote/*.fsproj"
    ++ "../src/Dap.Remote/*.fsproj"
    ++ "../src/Dap.Archive/*.fsproj"

[<EntryPoint>]
let main argv =
    argv
    |> Array.toList
    |> Context.FakeExecutionContext.Create false "build.fsx"
    |> Context.RuntimeContext.Fake
    |> Context.setExecutionContext
    NuGet.createAndRun NuGet.release feed projects
    0
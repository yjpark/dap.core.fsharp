#r "paket: groupref Build //"
#load ".fake/build.fsx/intellisense.fsx"

open Fake.Core
open Fake.IO.Globbing.Operators

module NuGet = Dap.Build.NuGet

let feed : NuGet.Feed = {
    NuGet.Source = "https://nuget.yjpark.org/nuget/dap"
    NuGet.ApiKey = NuGet.Plain "wnHZEG9N_OrmO3XKoAGT"
}

let projects =
    !! "src/Fable.Dap.Prelude/*.fsproj"
    ++ "src/Dap.Prelude/*.fsproj"
    ++ "src/Fable.Dap.Context/*.fsproj"
    ++ "src/Dap.Context/*.fsproj"
    ++ "src/Fable.Dap.Platform/*.fsproj"
    ++ "src/Dap.Platform/*.fsproj"
    ++ "src/Fable.Dap.WebSocket/*.fsproj"
    ++ "src/Dap.WebSocket/*.fsproj"
    ++ "src/Fable.Dap.Remote/*.fsproj"
    ++ "src/Dap.Remote/*.fsproj"
    ++ "src/Fable.Dap.Dsl/*.fsproj"
    ++ "src/Dap.Archive/*.fsproj"

NuGet.createAndRun NuGet.release feed projects

#r "paket: groupref Build //"
#load ".fake/build.fsx/intellisense.fsx"

open Fake.Core
open Fake.IO.Globbing.Operators

#load "src/Dap.Build/NuGet.fs"
module NuGet = Dap.Build.NuGet

let cleanDirs =
    !! "src/**/bin"
    ++ "src/**/obj"

let projects =
    !! "src/Dap.Build/*.fsproj"
    ++ "src/Dap.Prelude/*.fsproj"
    ++ "src/Dap.Prelude.Net/*.fsproj"
    ++ "src/Dap.Prelude.Fable/*.fsproj"
    ++ "src/Dap.Platform/*.fsproj"
    ++ "src/Dap.Platform.Net/*.fsproj"
    ++ "src/Dap.Platform.Fable/*.fsproj"
    ++ "src/Dap.WebSocket.Net/*.fsproj"
    ++ "src/Dap.WebSocket.Fable/*.fsproj"
    ++ "src/Dap.Remote/*.fsproj"
    ++ "src/Dap.Remote.Net/*.fsproj"
    ++ "src/Dap.Remote.Fable/*.fsproj"
    ++ "src/Dap.Archive.Net/*.fsproj"
    (*
    *)

let feed : NuGet.Feed = {
    NuGet.Source = "https://nuget.yjpark.org/nuget/dap"
    NuGet.ApiKey = NuGet.Environment "API_KEY_nuget_yjpark_org"
}

NuGet.run cleanDirs projects feed
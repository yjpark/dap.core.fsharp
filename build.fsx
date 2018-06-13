#r "paket: groupref Build //"
#load ".fake/build.fsx/intellisense.fsx"

open System.IO
open System.Text.RegularExpressions
open Fake.Core
open Fake.DotNet
open Fake.IO
open Fake.IO.Globbing.Operators
open Fake.IO.FileSystemOperators

type ApiKey =
    | Environment of string
    | Plain of string
    | NoAuth

type NuGetFeed = {
    Source : string
    ApiKey : ApiKey
}

let _localLigetFeed = {
    Source = "http://localhost:1281/api/v2"
    ApiKey = NoAuth
}

let yjparkFeed = {
    Source = "https://nuget.yjpark.org/nuget/dap"
    ApiKey = Environment "API_KEY_nuget_yjpark_org"
}

let feed = yjparkFeed

let cleanDirs =
    !! "src/**/bin"
    ++ "src/**/obj"

let projects =
    !! "src/Dap.Prelude/*.fsproj"
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

let checkVersion proj (releaseNotes : ReleaseNotes.ReleaseNotes) =
    let versionRegex = Regex("<Version>(.*?)</Version>", RegexOptions.IgnoreCase)
    File.ReadLines(proj)
    |> Seq.tryPick (fun line ->
        let m = versionRegex.Match(line)
        if m.Success then Some m else None)
    |> function
        | None -> failwith "Couldn't find version in project file"
        | Some m ->
            let version = m.Groups.[1].Value
            if version <> releaseNotes.NugetVersion then
                failwith <| sprintf "Mismatched version: project file => %s, RELEASE_NOTES.md => %s " version releaseNotes.NugetVersion
    releaseNotes

let loadReleaseNotes proj = 
    let dir = Path.GetDirectoryName(proj)
    dir </> "RELEASE_NOTES.md"
    |> ReleaseNotes.load
    |> checkVersion proj

let setPackParams proj (options : DotNet.PackOptions) =
    let releaseNotes = loadReleaseNotes proj
    let pkgReleaseNotes = sprintf "/p:PackageReleaseNotes=\"%s\"" (String.toLines releaseNotes.Notes)
    { options with
        Configuration = DotNet.Release
        Common =
            { options.Common with
                CustomParams = Some pkgReleaseNotes
                DotNetCliPath = "dotnet"
            }
    } 

let getNuGetApiKeyParam (apiKey : ApiKey) =
    match apiKey with
    | Environment key ->
        match Environment.environVarOrNone key with
        | Some key ->
            sprintf " -k %s" key
        | None ->
            failwith <| sprintf "Failed to get Api Key form environment: %s" key
    | Plain key ->
        sprintf " -k %s" key
    | NoAuth -> ""

let pushNuPkg (feed : NuGetFeed) proj =
    let dir = Path.GetDirectoryName(proj)
    let releaseNotes = loadReleaseNotes proj
    let mutable pkgPath = ""
    Directory.GetFiles(dir </> "bin" </> "Release", "*.nupkg")
    |> Array.find (fun pkg -> pkg.Contains(releaseNotes.NugetVersion))
    |> (fun pkg ->
        pkgPath <- pkg
        pkg
    )|> (fun pkg ->
        sprintf "push %s -s %s%s" pkg feed.Source <| getNuGetApiKeyParam feed.ApiKey
    )|> DotNet.exec id "nuget"
    |> fun result ->
        if not result.OK then
            failwith <| sprintf "Push nupkg failed: %s -> [%i] %A %A" pkgPath result.ExitCode result.Messages result.Errors

// *** Define Targets ***
Target.setLastDescription "Cleaning..."
Target.create "Clean" (fun _ ->
    cleanDirs
    |> Seq.iter (fun dir ->
        Trace.traceFAKE "Clean Dir: %s" dir
        Shell.cleanDir dir
    )
)

Target.setLastDescription "Restoring..."
Target.create "Restore" (fun _ ->
    projects
    |> Seq.iter (fun proj ->
        Trace.traceFAKE "Restore Project: %s" proj
        DotNet.restore id proj
    )
)

Target.setLastDescription "Building..."
Target.create "Build" (fun _ ->
    projects
    |> Seq.iter (fun proj ->
        Trace.traceFAKE "Build Project: %s" proj
        DotNet.build id proj
    )
)

Target.setLastDescription "Packing..."
Target.create "Pack" (fun _ ->
    projects
    |> Seq.iter (fun proj ->
        Trace.traceFAKE "Pack Project: %s" proj
        DotNet.pack (setPackParams proj) proj
    )
)

Target.setLastDescription "Publishing..."
Target.create "Publish" (fun _ ->
    projects
    |> Seq.iter (fun proj ->
        Trace.traceFAKE "Publish Project: %s" proj
        pushNuPkg feed proj
    )
)

open Fake.Core.TargetOperators

// *** Define Dependencies ***
"Clean"
    ==> "Restore"
    ==> "Build"
    ==> "Pack"
    ==> "Publish"

// *** Start Build ***
Target.runOrDefault "Build"

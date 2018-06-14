[<RequireQualifiedAccess>]
module Dap.Build.NuGet

open System.IO
open System.Text.RegularExpressions
open Fake.Core
open Fake.DotNet
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.Core.TargetOperators

[<Literal>]
let Clean = "Clean"

[<Literal>]
let Restore = "Restore"

[<Literal>]
let Build = "Build"

[<Literal>]
let Pack = "Pack"

[<Literal>]
let Publish = "Publish"

type ApiKey =
    | Environment of string
    | Plain of string
    | NoAuth

type Feed = {
    Source : string
    ApiKey : ApiKey
}

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

let private setPackParams proj (options : DotNet.PackOptions) =
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

let private getApiKeyParam (apiKey : ApiKey) =
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

let restore proj = 
    Trace.traceFAKE "Restore Project: %s" proj
    DotNet.restore id proj

let build proj = 
    Trace.traceFAKE "Build Project: %s" proj
    DotNet.build id proj

let pack proj = 
    Trace.traceFAKE "Pack Project: %s" proj
    DotNet.pack (setPackParams proj) proj

let publish (feed : Feed) proj =
    Trace.traceFAKE "Publish Project: %s" proj
    let dir = Path.GetDirectoryName(proj)
    let releaseNotes = loadReleaseNotes proj
    let mutable pkgPath = ""
    Directory.GetFiles(dir </> "bin" </> "Release", "*.nupkg")
    |> Array.find (fun pkg -> pkg.Contains(releaseNotes.NugetVersion))
    |> (fun pkg ->
        pkgPath <- pkg
        pkg
    )|> (fun pkg ->
        sprintf "push %s -s %s%s" pkg feed.Source <| getApiKeyParam feed.ApiKey
    )|> DotNet.exec id "nuget"
    |> fun result ->
        if not result.OK then
            failwith <| sprintf "Push nupkg failed: %s -> [%i] %A %A" pkgPath result.ExitCode result.Messages result.Errors

let createTargets cleanDirs projects feed =
    Target.setLastDescription "Cleaning..."
    Target.create Clean (fun _ ->
        cleanDirs
        |> Seq.iter (fun dir ->
            Trace.traceFAKE "Clean Dir: %s" dir
            Shell.cleanDir dir
        )
    )

    Target.setLastDescription "Restoring..."
    Target.create Restore (fun _ ->
        projects
        |> Seq.iter restore
    )

    Target.setLastDescription "Building..."
    Target.create Build (fun _ ->
        projects
        |> Seq.iter build
    )

    Target.setLastDescription "Packing..."
    Target.create Pack (fun _ ->
        projects
        |> Seq.iter pack
    )

    Target.setLastDescription "Publishing..."
    Target.create Publish (fun _ ->
        projects
        |> Seq.iter (publish feed)
    )

    // *** Define Dependencies ***
    Clean
        ==> Restore
        ==> Build
        ==> Pack
        ==> Publish
    |> ignore

let run cleanDirs projects feed =
    createTargets cleanDirs projects feed
    Target.runOrDefault Pack

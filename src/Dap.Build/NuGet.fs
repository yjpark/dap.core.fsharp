[<RequireQualifiedAccess>]
module Dap.Build.NuGet

open System
open System.IO
open System.Text.RegularExpressions
open Fake.Core
open Fake.DotNet
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.Core.TargetOperators
open Fake.IO

[<Literal>]
let Pack = "Pack"

[<Literal>]
let Publish = "Publish"

[<Literal>]
let Develop = "Develop"

[<Literal>]
let Inject = "Inject"

[<Literal>]
let Clear = "Clear"

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

let pack (config : DotNet.BuildConfiguration) proj = 
    Trace.traceFAKE "Pack Project: %s" proj
    let setOptions = fun (options : DotNet.PackOptions) ->
        let releaseNotes = loadReleaseNotes proj
        let pkgReleaseNotes = sprintf "/p:PackageReleaseNotes=\"%s\"" (String.toLines releaseNotes.Notes)
        { options with
            Configuration = config
            NoBuild = true
            Common =
                { options.Common with
                    CustomParams = Some pkgReleaseNotes
                    DotNetCliPath = "dotnet"
                }
        } 
    DotNet.pack setOptions proj

let publish (feed : Feed) proj =
    Trace.traceFAKE "Publish Project: %s" proj
    let dir = Path.GetDirectoryName(proj)
    let releaseNotes = loadReleaseNotes proj
    let mutable pkgPath = ""
    Directory.GetFiles(dir </> "bin" </> "Release", "*.nupkg")
    |> Array.find (fun pkg -> pkg.Contains(releaseNotes.NugetVersion))
    |> (fun pkg ->
        pkgPath <- pkg
        sprintf "push %s -s %s%s" pkg feed.Source <| getApiKeyParam feed.ApiKey
    )|> DotNet.exec id "nuget"
    |> fun result ->
        if not result.OK then
            failwith <| sprintf "Push nupkg failed: %s -> [%i] %A %A" pkgPath result.ExitCode result.Messages result.Errors

let createTargets (config : DotNet.BuildConfiguration) projects =
    Dap.Build.DotNet.createTargets config projects
    Target.setLastDescription "Packing..."
    Target.create Pack (fun _ ->
        projects
        |> Seq.iter (pack config)
    )
    Dap.Build.DotNet.Build
        ==> Pack
    |> ignore
let homePath =
    match Environment.OSVersion.Platform with
    | PlatformID.Unix | PlatformID.MacOSX -> Environment.GetEnvironmentVariable("HOME")
    | _ -> Environment.ExpandEnvironmentVariables("%HOMEDRIVE%%HOMEPATH%");
let getNugetCachePath (package : string) (version : string option) =
    let path = Path.Combine [| homePath ; ".nuget" ; "packages" ; (package.ToLower ()) |]
    version
    |> Option.map (fun version ->
        Path.Combine [| path ; version |]
    )|> Option.defaultValue path

let doInject (package : string) (version : string) (pkg : string) =
    let path = getNugetCachePath package <| Some version
    Directory.ensure path
    Shell.cleanDir path
    Shell.copyFile path pkg
    Zip.unzip path pkg
    File.writeNew (Path.Combine [| path ; "Dap.Build_Inject.txt" |]) [
        sprintf "Injected At: %A" System.DateTime.Now 
        pkg
    ]
    Trace.traceFAKE "    -> %s/%s" path <| Path.GetFileName pkg

let inject (config : DotNet.BuildConfiguration) proj =
    Trace.traceFAKE "Inject Project: %s" proj
    let dir = Path.GetDirectoryName(proj)
    let package = Path.GetFileName(dir)
    let releaseNotes = loadReleaseNotes proj
    let folder = Dap.Build.DotNet.getConfigFolder config
    Directory.GetFiles(dir </> "bin" </> folder, "*.nupkg")
    |> Array.find (fun pkg -> pkg.Contains(releaseNotes.NugetVersion))
    |> doInject package releaseNotes.NugetVersion

let run projects feed =
    createTargets DotNet.Release projects
    Target.setLastDescription "Injecting to Local NuGet Cache..."
    Target.create Inject (fun _ ->
        projects
        |> Seq.iter (inject DotNet.Debug)
    )
    Target.setLastDescription "Publishing..."
    Target.create Publish (fun _ ->
        projects
        |> Seq.iter (publish feed)
    )
    Pack
        ==> Inject
        ==> Publish
    |> ignore
    Target.runOrDefault Inject

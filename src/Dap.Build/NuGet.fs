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

module DapDotNet = Dap.Build.DotNet

[<Literal>]
let Pack = "Pack"

[<Literal>]
let Publish = "Publish"

[<Literal>]
let Develop = "Develop"

[<Literal>]
let Inject = "Inject"

[<Literal>]
let Recover = "Recover"

type ApiKey =
    | Environment of string
    | Plain of string
    | NoAuth

type Feed = {
    Source : string
    ApiKey : ApiKey
}

type Options = {
    DotNet : DapDotNet.Options
    CreateInjectTargets : bool
}

let debug = {
    DotNet = DapDotNet.debug
    CreateInjectTargets = true
}

let release = {
    DotNet = DapDotNet.release
    CreateInjectTargets = true
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

let pack (options : Options) proj = 
    Trace.traceFAKE "Pack Project: %s" proj
    let setOptions = fun (options' : DotNet.PackOptions) ->
        let releaseNotes = loadReleaseNotes proj
        let pkgReleaseNotes = sprintf "/p:PackageReleaseNotes=\"%s\"" (String.toLines releaseNotes.Notes)
        { options' with
            Configuration = options.DotNet.Configuration
            NoBuild = true
            Common =
                { options'.Common with
                    CustomParams = Some pkgReleaseNotes
                    DotNetCliPath = "dotnet"
                }
        } 
    DotNet.pack setOptions proj

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

let getOriginalNugetCachePath (package : string) (version : string) =
    getNugetCachePath package <| Some (version + "-original")

let getSha512Stream (stream:Stream) =
    use hasher = System.Security.Cryptography.SHA512.Create() :> System.Security.Cryptography.HashAlgorithm
    Convert.ToBase64String(hasher.ComputeHash(stream))

let getSha512File (filePath:string) =
    use stream = File.OpenRead(filePath)
    getSha512Stream stream

let doInject (package : string) (version : string) (pkg : string) =
    let path = getNugetCachePath package <| Some version
    Directory.ensure path
    let nupkgPath = Path.GetFileName(pkg)
    let hashPath = Path.Combine [| path ; nupkgPath + ".sha512" |]
    let injectPath = Path.Combine [| path ; "Dap.Build_Inject.txt" |]
    if not (File.exists injectPath) then
        let originalPath = getOriginalNugetCachePath package version
        Shell.copyDir originalPath path (fun _ -> true)
    Shell.cleanDir path
    Shell.copyFile path pkg
    let hash = getSha512File pkg
    File.writeNew hashPath [hash]
    Zip.unzip path pkg
    File.writeNew injectPath [
        sprintf "Injected At: %A" System.DateTime.Now 
        sprintf "SHA512 Hash: %s" hash
        pkg
    ]
    Trace.traceFAKE "    -> %s/%s" path <| Path.GetFileName pkg

let inject (options : Options) proj =
    Trace.traceFAKE "Inject Project: %s" proj
    let dir = Path.GetDirectoryName(proj)
    let package = Path.GetFileName(dir)
    let releaseNotes = loadReleaseNotes proj
    let folder = DapDotNet.getConfigFolder options.DotNet.Configuration
    Directory.GetFiles(dir </> "bin" </> folder, "*.nupkg")
    |> Array.find (fun pkg -> pkg.Contains(releaseNotes.NugetVersion))
    |> doInject package releaseNotes.NugetVersion

let doRecover (package : string) (version : string) =
    let path = getNugetCachePath package <| Some version
    let originalPath = getOriginalNugetCachePath package version
    if DirectoryInfo.exists (DirectoryInfo.ofPath originalPath) then
        Shell.cleanDir path
        Shell.copyDir path originalPath (fun _ -> true)
        Trace.traceFAKE "    -> %s" path

let recover proj =
    Trace.traceFAKE "Recover Project: %s" proj
    let dir = Path.GetDirectoryName(proj)
    let package = Path.GetFileName(dir)
    let releaseNotes = loadReleaseNotes proj
    doRecover package releaseNotes.NugetVersion

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
            failwith <| sprintf "Push nupkg Failed: %s -> [%i] %A %A" pkgPath result.ExitCode result.Messages result.Errors

let createTargets' (options : Options) noPrefix feed projects =
    let (label, prefix) = DapDotNet.createTargets' options.DotNet noPrefix projects
    Target.setLastDescription <| sprintf "Pack %s" label
    Target.create (prefix + Pack) (fun _ ->
        projects
        |> Seq.iter (pack options)
    )
    Target.setLastDescription <| sprintf "Publish %s" label
    Target.create (prefix + Publish) (fun _ ->
        projects
        |> Seq.iter (publish feed)
    )
    |> ignore
    if options.CreateInjectTargets then
        Target.setLastDescription <| sprintf "Inject %s" label
        Target.create (prefix + Inject) (fun _ ->
            projects
            |> Seq.iter (inject options)
        )
        Target.setLastDescription <| sprintf "Recover %s" label
        Target.create (prefix + Recover) (fun _ ->
            projects
            |> Seq.iter recover
        )
        prefix + DapDotNet.Build
            ==> prefix + Pack
            ==> prefix + Inject
            ==> prefix + Publish
    else
        prefix + DapDotNet.Build
            ==> prefix + Pack
            ==> prefix + Publish
    |> ignore

let createTargets options =
    createTargets' options true

let createPerProjectTargets options feed proj =
    createTargets' options false feed [proj]

let create (options : Options) feed projects =
    createTargets options feed projects
    if options.DotNet.CreatePerProjectTargets && Seq.length projects > 1 then
        projects
        |> Seq.iter (createPerProjectTargets options feed)

let createAndRun (options : Options) feed projects =
    create options feed projects
    Target.runOrDefault Inject

[<RequireQualifiedAccess>]
module Dap.Build.DotNet

open System
open System.IO
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

type Options = {
    UseDebugConfig : bool
    CreatePerProjectTargets : bool
} with
    member this.Configuration =
        if this.UseDebugConfig then DotNet.Debug else DotNet.Release

let debug = {
    UseDebugConfig = true
    CreatePerProjectTargets = true
}

let release = {
    UseDebugConfig = false
    CreatePerProjectTargets = true
}

let getConfigFolder (config : DotNet.BuildConfiguration) =
    match config with
    | DotNet.Debug -> "Debug"
    | DotNet.Release -> "Release"
    | DotNet.Custom config -> config

let getPackage proj =
    let dir = Path.GetDirectoryName(proj)
    Path.GetFileName(dir)

let clean (options : Options) proj =
    Trace.traceFAKE "Clean Project: %s" proj
    (*
    //Dotnet clean might left some extra files (e.g .nuspec) which
    //will break pack process
    let setOptions = fun (options' : DotNet.Options) ->
        { options' with
            WorkingDirectory = Path.GetDirectoryName(proj)
        } 
    sprintf "--configuration %s" <| getConfigFolder config
    |> DotNet.exec setOptions "clean"
    |> ignore
     *)
    let dir = Path.GetDirectoryName(proj)
    let folder = getConfigFolder options.Configuration
    Shell.cleanDirs [
        Path.Combine [| dir ; "bin" ; folder |]
        Path.Combine [| dir ; "obj" ; folder |]
    ]

let restore proj = 
    Trace.traceFAKE "Restore Project: %s" proj
    let setOptions = fun (options' : DotNet.RestoreOptions) ->
        { options' with
            Common =
                { options'.Common with
                    CustomParams = Some "--no-dependencies"
                }
        } 
    DotNet.restore setOptions proj

let build (options : Options) (noDependencies : bool) proj = 
    Trace.traceFAKE "Build Project: %s" proj
    let setOptions = fun (options' : DotNet.BuildOptions) ->
        let mutable param = "--no-restore"
        if noDependencies then
            param <- sprintf "%s --no-dependencies" param
        { options' with
            Configuration = options.Configuration
            Common =
                { options'.Common with
                    CustomParams = Some param
                }
        } 
    DotNet.build setOptions proj

let getLabelAndPrefix (noPrefix : bool) (projects : seq<string>) =
    let len = Seq.length projects
    if len = 1 then
        let dir = Path.GetDirectoryName(Seq.head projects)
        let label = Path.GetFileName(dir)
        let prefix = if noPrefix then "" else label + ":"
        (label, prefix)
    else
        (sprintf "%i Projects" len, "")

let createTargets' (options : Options) (noPrefix : bool) (projects : seq<string>) =
    let (label, prefix) = getLabelAndPrefix noPrefix projects
    Target.setLastDescription <| sprintf "Clean %s" label
    Target.create (prefix + Clean) (fun _ ->
        projects
        |> Seq.iter (clean options)
    )
    Target.setLastDescription <| sprintf "Restore %s" label
    Target.create (prefix + Restore) (fun _ ->
        projects
        |> Seq.iter restore
    )
    Target.setLastDescription <| sprintf "Build %s" label
    Target.create (prefix + Build) (fun _ ->
        projects
        |> Seq.iteri (fun i proj -> 
            build options (i > 0) proj
        )
    )
    prefix + Clean
        ==> prefix + Restore
        ==> prefix + Build
    |> ignore
    (label, prefix)

let createTargets options projects =
    createTargets' options true projects
    |> ignore

let createPerProjectTargets options proj =
    createTargets' options false [proj]
    |> ignore

let run (options : Options) projects =
    createTargets options projects
    if options.CreatePerProjectTargets && Seq.length projects > 1 then
        projects
        |> Seq.iter (createPerProjectTargets options)
    Target.runOrDefault Build

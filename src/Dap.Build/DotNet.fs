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

let getConfigFolder (config : DotNet.BuildConfiguration) =
    match config with
    | DotNet.Debug -> "Debug"
    | DotNet.Release -> "Release"
    | DotNet.Custom config -> config

let getPackage proj =
    let dir = Path.GetDirectoryName(proj)
    Path.GetFileName(dir)

let clean (config : DotNet.BuildConfiguration) proj =
    Trace.traceFAKE "Clean Project: %s" proj
    (*
    //Dotnet clean might left some extra files (e.g .nuspec) which
    //will break pack process
    let setOptions = fun (options : DotNet.Options) ->
        { options with
            WorkingDirectory = Path.GetDirectoryName(proj)
        } 
    sprintf "--configuration %s" <| getConfigFolder config
    |> DotNet.exec setOptions "clean"
    |> ignore
     *)
    let dir = Path.GetDirectoryName(proj)
    let folder = getConfigFolder config
    Shell.cleanDirs [
        Path.Combine [| dir ; "bin" ; folder |]
        Path.Combine [| dir ; "obj" ; folder |]
    ]

let restore proj = 
    Trace.traceFAKE "Restore Project: %s" proj
    let setOptions = fun (options : DotNet.RestoreOptions) ->
        { options with
            Common =
                { options.Common with
                    CustomParams = Some "--no-dependencies"
                }
        } 
    DotNet.restore setOptions proj

let build (config : DotNet.BuildConfiguration) (noDependencies : bool) proj = 
    Trace.traceFAKE "Build Project: %s" proj
    let setOptions = fun (options : DotNet.BuildOptions) ->
        let mutable param = "--no-restore"
        if noDependencies then
            param <- sprintf "%s --no-dependencies" param
        { options with
            Configuration = config
            Common =
                { options.Common with
                    CustomParams = Some param
                }
        } 
    DotNet.build setOptions proj

let createTargets (config : DotNet.BuildConfiguration) projects =
    Target.setLastDescription <| sprintf "Clean %i Projects" (Seq.length projects)
    Target.create Clean (fun _ ->
        projects
        |> Seq.iter (clean config)
    )

    Target.setLastDescription <| sprintf "Restore %i Projects" (Seq.length projects)
    Target.create Restore (fun _ ->
        projects
        |> Seq.iter restore
    )

    Target.setLastDescription <| sprintf "Build %i Projects" (Seq.length projects)
    Target.create Build (fun _ ->
        projects
        |> Seq.iter (build config true)
    )

    // *** Define Dependencies ***
    Clean
        ==> Restore
        ==> Build
    |> ignore

let buildProject (config : DotNet.BuildConfiguration) proj =
    let package = getPackage proj
    Target.setLastDescription <| sprintf "Build %s" package
    Target.create package (fun _ ->
        build config false proj
    )
    Restore ==> package |> ignore

let createProjectTargets (config : DotNet.BuildConfiguration) projects =
    projects
    |> Seq.iter (buildProject config)

let run projects =
    createTargets DotNet.Release projects
    createProjectTargets DotNet.Release projects
    Target.runOrDefault Build

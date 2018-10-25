[<AutoOpen>]
module Dap.Platform.Generator.Pack

open Dap.Prelude
open Dap.Context.Meta
open Dap.Context.Generator
open Dap.Context.Generator.Util
open Dap.Platform.Meta

let private aliasesToLines (aliases : (string * string) list) =
    aliases
    |> List.map (fun alias ->
        sprintf "module %s = %s" (fst alias) (snd alias)
    )

let private getServiceAliases (service : AgentMeta) =
    aliasesToLines service.Aliases

let private getSpawnerAliases (spawner : AgentMeta) =
    aliasesToLines spawner.Aliases

let private getExtraArgsAliases (args : ExtraArgsMeta) =
    aliasesToLines args.Aliases

let rec getAliases (includeParents : bool) (meta : PackMeta) =
    let fromParents =
        if includeParents then
            meta.Parents
            |> List.map snd
            |> List.map ^<| getAliases includeParents
            |> List.concat
        else
            []
    [
        fromParents
        meta.Services |> List.map getServiceAliases |> List.concat
        meta.Spawners |> List.map getSpawnerAliases |> List.concat
        meta.ExtraArgs |> List.map getExtraArgsAliases |> List.concat
    ]|> List.concat

let getAsPackName (packName : string) =
    if packName.StartsWith ("I") then
        packName.Substring (1, packName.Length - 1)
    else
        packName
    |> sprintf "As%s"

let getArgsMemberName (name : string) =
    let name = name.AsCodeMemberName
#if FABLE_COMPILER
    //Fable records not allow same name for field and member
    let name = sprintf "%s'" name
#endif
    name

let mutable private processedPacks : Set<string> = Set.empty
let clearProcessedPacks () =
    processedPacks <- Set.empty
let markPackProcessed (pack : string) =
    processedPacks <- processedPacks |> Set.add pack
let didPackProcessed (pack : string) =
    processedPacks |> Set.contains pack

type InterfaceGenerator (meta : PackMeta) =
    let getArgsInterfaceHeader (param : PackParam) =
        [
            sprintf "type %sArgs =" param.Name
        ]
    let getArgsServiceMember (service : AgentMeta) =
        let name = sprintf "%s%s" service.Key service.Kind
        sprintf "    abstract %s : %s with get" (getArgsMemberName name) service.Args.Type
    let getArgsSpawnerMember (spawner : AgentMeta) =
        sprintf "    abstract %s : %s with get" (getArgsMemberName spawner.Kind) spawner.Args.Type
    let getArgsExtraMember (args : ExtraArgsMeta) =
        sprintf "    abstract %s : %s with get" (getArgsMemberName args.Key) args.Args.Type
    let getArgsParentInherit ((name, _package) : string * PackMeta) =
        sprintf "    inherit %sArgs" name
    let getArgsParentAs ((name, _package) : string * PackMeta) =
        sprintf "    abstract %sArgs : %sArgs with get" (getAsPackName name) name
    let getInterfaceHeader (param : PackParam) =
        [
            sprintf "type %s =" param.Name
            sprintf "    inherit IPack"
        ]
    let getInterfaceMiddle (param : PackParam) =
        [
        #if !FABLE_COMPILER
            yield sprintf "    abstract Args : %sArgs with get" param.Name
        #endif
        ]
    let getServiceMember (service : AgentMeta) =
        let name = sprintf "%s%s" service.Key service.Kind
        sprintf "    abstract %s : %s with get" name.AsCodeMemberName service.Type
    let getSpawnerMember (spawner : AgentMeta) =
        let kind = spawner.Kind.AsCodeMemberName
        sprintf "    abstract Get%sAsync : Key -> Task<%s * bool>" kind spawner.Type
    let getParentInherit ((name, _package) : string * PackMeta) =
        sprintf "    inherit %s" name
    let getParentAs ((name, _package) : string * PackMeta) =
        sprintf "    abstract %s : %s with get" (getAsPackName name) name
    interface IGenerator<PackParam> with
        member this.Generate param =
            [
                getAliases false meta
                getArgsInterfaceHeader param
                meta.Parents |> List.map getArgsParentInherit
                meta.Services |> List.map getArgsServiceMember
                meta.Spawners |> List.map getArgsSpawnerMember
                meta.ExtraArgs |> List.map getArgsExtraMember
                meta.Parents |> List.map getArgsParentAs
                [""]
                getInterfaceHeader param
                meta.Parents |> List.map getParentInherit
                getInterfaceMiddle param
                meta.Services |> List.map getServiceMember
                meta.Spawners |> List.map getSpawnerMember
                meta.Parents |> List.map getParentAs
            ]|> List.concat
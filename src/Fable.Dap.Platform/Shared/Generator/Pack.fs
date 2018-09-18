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

let private getServiceAliases (service : ServiceMeta) =
    aliasesToLines service.Aliases

let private getSpawnerAliases (spawner : SpawnerMeta) =
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

type InterfaceGenerator (meta : PackMeta) =
    let getArgsInterfaceHeader (param : PackParam) =
        [
            sprintf "type %sArgs =" param.Name
        ]
    let getArgsServiceMember (service : ServiceMeta) =
        let name = sprintf "%s%s" service.Key service.Kind
        sprintf "    abstract %s : %s with get" name.AsCodeMemberName service.Args.Type
    let getArgsSpawnerMember (spawner : SpawnerMeta) =
        let name = spawner.Kind.AsCodeMemberName
        sprintf "    abstract %s : %s with get" name spawner.Args.Type
    let getArgsExtraMember (args : ExtraArgsMeta) =
        let name = args.Key.AsCodeMemberName
        sprintf "    abstract %s : %s with get" name args.Args.Type
    let getArgsParentInherit ((name, _package) : string * PackMeta) =
        sprintf "    inherit %sArgs" name
    let getInterfaceHeader (param : PackParam) =
        [
            sprintf "type %s =" param.Name
            sprintf "    inherit ILogger"
        ]
    let getInterfaceMiddle (param : PackParam) =
        [
            sprintf "    abstract Env : IEnv with get"
            sprintf "    abstract Args : %sArgs with get" param.Name
        ]
    let getServiceMember (service : ServiceMeta) =
        let name = sprintf "%s%s" service.Key service.Kind
        sprintf "    abstract %s : %s with get" name.AsCodeMemberName service.Type
    let getSpawnerMember (spawner : SpawnerMeta) =
        let kind = spawner.Kind.AsCodeMemberName
        sprintf "    abstract Get%sAsync : Key -> Task<%s * bool>" kind spawner.Type
    let getParentInherit ((name, _package) : string * PackMeta) =
        sprintf "    inherit %s" name
    let rec isEmptyInterface (pack : PackMeta) (includeExtraArgs : bool) =
        let mutable isEmpty = true
        if pack.Services.Length > 0 then
            isEmpty <- false
        elif pack.Spawners.Length > 0 then
            isEmpty <- false
        elif includeExtraArgs && pack.ExtraArgs.Length > 0 then
            isEmpty <- false
        else
            for (_name, parent) in pack.Parents do
                if isEmpty then
                    if isEmptyInterface parent includeExtraArgs then
                        ()
                    else
                        isEmpty <- false
        isEmpty
    interface IGenerator<PackParam> with
        member this.Generate param =
            [
                getAliases false meta
                getArgsInterfaceHeader param
                meta.Parents |> List.map getArgsParentInherit
                meta.Services |> List.map getArgsServiceMember
                meta.Spawners |> List.map getArgsSpawnerMember
                meta.ExtraArgs |> List.map getArgsExtraMember
                (if isEmptyInterface meta true then
                    ["    end"]
                else
                    [])
                [""]
                getInterfaceHeader param
                meta.Parents |> List.map getParentInherit
                getInterfaceMiddle param
                meta.Services |> List.map getServiceMember
                meta.Spawners |> List.map getSpawnerMember
            ]|> List.concat
[<AutoOpen>]
module Dap.Platform.Generator.App

open Dap.Prelude
open Dap.Context.Meta
open Dap.Context.Generator
open Dap.Context.Generator.Util
open Dap.Platform.Meta

let private getAsPackName (packName : string) =
    if packName.StartsWith ("I") then
        packName.Substring (1, packName.Length - 1)
    else
        packName
    |> sprintf "As%s"

let getAliases (meta : AppMeta) =
    meta.Packs
    |> List.map snd
    |> List.map ^<| Pack.getAliases true
    |> List.concat

type InterfaceGenerator (meta : AppMeta) =
    let getArgsMeta packName (args : ArgsMeta) (name : string) =
        let key = name.AsCodeJsonKey
        [
            match args with
            | JsonArgs name ->
                let initValue = sprintf "(%s.Default ())" name
                M.custom (name, key, initValue)
            | CodeArgs (name, code) ->
                M.hardcoded (name, key, code)
            |> M.comment packName
        ]
    let getServiceArgsMeta packName (service : ServiceMeta) =
        getArgsMeta packName service.Args <| sprintf "%s%s" service.Key service.Kind
    let getSpawnerArgsMeta packName (spawner : SpawnerMeta) =
        getArgsMeta packName spawner.Args spawner.Kind
    let getExtraArgsMeta packName (args : ExtraArgsMeta) =
        getArgsMeta packName args.Args args.Key
    let rec getPackArgsMeta ((name, package) : string * PackMeta) =
        (
            package.Parents
            |> List.map ^<| getPackArgsMeta
            |> List.concat
        ) @ (
            package.Services
            |> List.map ^<| getServiceArgsMeta name
            |> List.concat
        ) @ (
            package.Spawners
            |> List.map ^<| getSpawnerArgsMeta name
            |> List.concat
        ) @ (
            package.ExtraArgs
            |> List.map ^<| getExtraArgsMeta name
            |> List.concat
        )
    let getServiceArgsMember (packName : string) (service : ServiceMeta) =
        let name = sprintf "%s%s" service.Key service.Kind
        let name = name.AsCodeMemberName
        [
            sprintf "        member this.%s (* %s *) : %s = this.%s" name packName service.Args.Type name
        ]
    let getSpawnerArgsMember (packName : string) (spawner : SpawnerMeta) =
        let name = spawner.Kind.AsCodeMemberName
        [
            sprintf "        member this.%s (* %s *) : %s = this.%s" name packName spawner.Args.Type name
        ]
    let getExtraArgsMember (packName : string) (args : ExtraArgsMeta) =
        let name = args.Key.AsCodeMemberName
        [
            sprintf "        member this.%s (* %s *) : %s = this.%s" name packName args.Args.Type name
        ]
    let rec getPackArgsMembers (names : string list) ((name, package) : string * PackMeta) =
        let isEmpty = package.Services.Length = 0 && package.Spawners.Length = 0 && package.ExtraArgs.Length = 0
        [
            sprintf "    interface %sArgs%s" name (if isEmpty then "" else " with")
        ] @ (
            package.Services
            |> List.map ^<| getServiceArgsMember name
            |> List.concat
        ) @ (
            package.Spawners
            |> List.map ^<| getSpawnerArgsMember name
            |> List.concat
        ) @ (
            package.ExtraArgs
            |> List.map ^<| getExtraArgsMember name
            |> List.concat
        ) @ (
            package.Parents
            |> List.map ^<| getPackArgsMembers ^<| names @ [name]
            |> List.concat
        ) @ [
            sprintf "    member this.%sArgs = this :> %sArgs" (getAsPackName name) name
        ]
    let getArgsClassAndBuilder (param : AppParam) =
        let kind = sprintf "%sArgs" param.Name
        [
            meta.Packs |> List.map getPackArgsMeta |> List.concat
        ]|> List.concat
        |> fun fields ->
            let argsMeta =
                fields
                |> List.map (fun f -> f :> IPropMeta)
                |> ComboMeta.Create []
            [
                G.LooseJsonRecord (kind, [], argsMeta)
                meta.Packs |> List.map (getPackArgsMembers []) |> List.concat
                [""]
                G.ValueBuilder (kind, argsMeta)
            ] |> List.concat
    let getInterfaceHeader (param : AppParam) =
        [
            sprintf "type I%s =" param.Name
            sprintf "    inherit ILogger"
            sprintf "    abstract Env : IEnv with get"
            sprintf "    abstract Args : %sArgs with get" param.Name
        ]
    let getPackInherit ((name, _package) : string * PackMeta) =
        sprintf "    inherit %s" name
    interface IGenerator<AppParam> with
        member this.Generate param =
            [
                getAliases meta
                getArgsClassAndBuilder param
                [""]
                getInterfaceHeader param
                meta.Packs |> List.map getPackInherit
            ]|> List.concat

type ClassGenerator (meta : AppMeta) =
    let getClassHeader (param : AppParam) =
        [
            yield sprintf "type %s (logging : ILogging, scope : Scope) =" param.Name
        #if FABLE_COMPILER
            yield sprintf "    let env = %s logging scope" meta.Kind
        #else
            yield sprintf "    let env = %s %s logging scope" meta.Kind meta.Platform
        #endif
            yield sprintf "    let mutable args : %sArgs option = None" param.Name
            yield sprintf "    let mutable setupError : exn option = None"
        ]
    let getServiceField (packName : string) (service : ServiceMeta) =
        let name = sprintf "%s%s" service.Key service.Kind
        [
            sprintf "    let mutable (* %s *) %s : %s option = None" packName name.AsCodeVariableName service.Type
        ]
    let rec getPackFields ((name, package) : string * PackMeta) =
        (
            package.Parents
            |> List.map getPackFields
            |> List.concat
        ) @ (
            package.Services
            |> List.map ^<| getServiceField name
            |> List.concat
        )
    let getServiceMember (packName : string) (service : ServiceMeta) =
        let name = sprintf "%s%s" service.Key service.Kind
        [
            sprintf "        member __.%s (* %s *) : %s = %s |> Option.get" name.AsCodeMemberName packName service.Type name.AsCodeVariableName
        ]
    let getSpawnerMember (packName : string) (spawner : SpawnerMeta) =
        let kind = spawner.Kind.AsCodeMemberName
        [
            sprintf "        member __.Get%sAsync (key : Key) (* %s *) : Task<%s * bool> = task {" kind packName spawner.Type
            sprintf "            let! (agent, isNew) = env.HandleAsync <| DoGetAgent \"%s\" key" kind
            sprintf "            return (agent :?> %s, isNew)" spawner.Type
            sprintf "        }"
        ]
    let getAsPackCode (packName : string) =
        getAsPackName packName
        |> sprintf "this.%s"
    let rec getPackMembers (names : string list) ((name, package) : string * PackMeta) =
        [
            sprintf "    interface %s with" name
            sprintf "        member __.Args = (Option.get args) .%sArgs" <| getAsPackName name
        ] @ (
            package.Services
            |> List.map ^<| getServiceMember name
            |> List.concat
        ) @ (
            package.Spawners
            |> List.map ^<| getSpawnerMember name
            |> List.concat
        ) @ (
            package.Parents
            |> List.map ^<| getPackMembers ^<| names @ [name]
            |> List.concat
        ) @ [
            sprintf "    member %s = this :> %s" (getAsPackCode name) name
        ]
    let getClassMiddle (param : AppParam) =
        [
            yield sprintf "    static member Create logging scope = new %s (logging, scope)" param.Name
            yield sprintf "    abstract member SetupExtrasAsync : unit -> Task<unit>"
            yield sprintf "    default __.SetupExtrasAsync () = task {"
            yield sprintf "        return ()"
            yield sprintf "    }"
        ]
    let getPackArgs (pack : string option) =
        pack
        |> Option.map getAsPackCode
        |> Option.map ^<| sprintf "%s "
        |> Option.defaultValue ""
    let getServiceSetup (packName : string) (service : ServiceMeta) =
        let name = sprintf "%s%s" service.Key service.Kind
        let varName = name.AsCodeVariableName
        [
            yield sprintf "                let! (* %s *) %s' = env |> Env.addServiceAsync (%s %sargs'.%s) \"%s\" \"%s\""
                packName varName service.Spec (getPackArgs service.Pack) name.AsCodeMemberName service.Kind service.Key
            if service.Type.StartsWith ("IAgent") then
                yield sprintf "                %s <- Some (%s' :> %s)" varName varName service.Type
            else
                yield sprintf "                %s <- Some %s'" varName varName
        ]
    let getSpawnerSetup (packName : string) (spawner : SpawnerMeta) =
        [
            sprintf "                do! env |> Env.registerAsync (%s (* %s *) %sargs'.%s) \"%s\""
                spawner.Spec packName (getPackArgs spawner.Pack) spawner.Kind spawner.Kind
        ]
    let rec getPackSetups (names : string list) ((name, package) : string * PackMeta) =
        (
            package.Parents
            |> List.map ^<| getPackSetups ^<| names @ [name]
            |> List.concat
        ) @ (
            package.Services
            |> List.map ^<| getServiceSetup name
            |> List.concat
        ) @ (
            package.Spawners
            |> List.map ^<| getSpawnerSetup name
            |> List.concat
        )
    let getSetupAsync (param : AppParam) =
        [
            [
                sprintf "    member this.SetupAsync (getArgs : unit -> %sArgs) : Task<unit> = task {" param.Name
                sprintf "        if args.IsNone then"
                sprintf "            let args' = getArgs ()"
                sprintf "            args <- Some args'"
                sprintf "            try"
            ]
            meta.Packs |> List.map (getPackSetups []) |> List.concat
            [
                sprintf "                do! this.SetupExtrasAsync ()"
                sprintf "                logInfo env \"%s.SetupAsync\" \"Setup_Succeed\" (E.encodeJson 4 args')" param.Name
                sprintf "            with e ->"
                sprintf "                setupError <- Some e"
                sprintf "                logException env \"%s.SetupAsync\" \"Setup_Failed\" (E.encodeJson 4 args') e" param.Name
                sprintf "        else"
                sprintf "            logError env \"%s.SetupAsync\" \"Already_Setup\" (args, setupError, getArgs)" param.Name
                sprintf "    }"
                sprintf "    member this.Setup (callback : I%s -> unit) (getArgs : unit -> %sArgs) : I%s =" param.Name param.Name param.Name
                sprintf "        if args.IsSome then"
                sprintf "            failWith \"Already_Setup\" <| E.encodeJson 4 args.Value"
                sprintf "        env.RunTask0 raiseOnFailed (fun _ -> task {"
                sprintf "            do! this.SetupAsync getArgs"
                sprintf "            match setupError with"
                sprintf "            | None -> callback this.As%s" param.Name
                sprintf "            | Some e -> raise e"
                sprintf "        })"
                sprintf "        this.As%s" param.Name
                sprintf "    member this.SetupArgs (callback : I%s -> unit) (args' : %sArgs) : I%s =" param.Name param.Name param.Name
                sprintf "        fun () -> args'"
                sprintf "        |> this.Setup callback"
                sprintf "    member this.SetupJson (callback : I%s -> unit) (args' : Json) : I%s =" param.Name param.Name
                sprintf "        fun () ->"
                sprintf "            try"
                sprintf "                castJson %sArgs.JsonDecoder args'" param.Name
                sprintf "            with e ->"
                sprintf "                logException env \"%s.Setup\" \"Decode_Failed\" args e" param.Name
                sprintf "                raise e"
                sprintf "        |> this.Setup callback"
                sprintf "    member this.SetupText (callback : I%s -> unit) (args' : string) : I%s =" param.Name param.Name
                sprintf "        parseJson args'"
                sprintf "        |> this.SetupJson callback"
                sprintf "    member __.SetupError : exn option = setupError"
                sprintf "    interface I%s with" param.Name
                sprintf "        member __.Env : IEnv = env"
                sprintf "        member __.Args : %sArgs = args |> Option.get" param.Name
            ]
        ]|> List.concat
    let getClassFooter (param : AppParam) =
        [
            sprintf "    interface ILogger with"
            sprintf "        member __.Log m = env.Log m"
            sprintf "    member this.As%s = this :> I%s" param.Name param.Name
        ]
    interface IGenerator<AppParam> with
        member this.Generate param =
            [
                getAliases meta
                getClassHeader param
                meta.Packs |> List.map getPackFields |> List.concat
                getClassMiddle param
                getSetupAsync param
                meta.Packs |> List.map (getPackMembers []) |> List.concat
                getClassFooter param
            ]|> List.concat


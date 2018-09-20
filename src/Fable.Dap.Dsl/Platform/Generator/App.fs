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
        if didPackProcessed name then
            []
        else
            markPackProcessed name
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
        sprintf "        member this.%s (* %s *) : %s = this.%s" (getArgsMemberName name) packName service.Args.Type name
    let getSpawnerArgsMember (packName : string) (spawner : SpawnerMeta) =
        let name = spawner.Kind.AsCodeMemberName
        sprintf "        member this.%s (* %s *) : %s = this.%s" (getArgsMemberName name) packName spawner.Args.Type name
    let getExtraArgsMember (packName : string) (args : ExtraArgsMeta) =
        let name = args.Key.AsCodeMemberName
        sprintf "        member this.%s (* %s *) : %s = this.%s" (getArgsMemberName name) packName args.Args.Type name
    let getPackArgsAs (names : string list) ((name, package) : string * PackMeta) =
        let asName = getAsPackName name
        sprintf "        member this.%sArgs = this.%sArgs" asName asName
    let rec getPackArgsMembers (names : string list) ((name, package) : string * PackMeta) =
        if didPackProcessed name then
            []
        else
            markPackProcessed name
            let names = names @ [name]
            (
                package.Parents
                |> List.map ^<| getPackArgsMembers names
                |> List.concat
            ) @ [
                sprintf "    interface %sArgs with" name
            ] @ (
                package.Services
                |> List.map ^<| getServiceArgsMember name
            ) @ (
                package.Spawners
                |> List.map ^<| getSpawnerArgsMember name
            ) @ (
                package.ExtraArgs
                |> List.map ^<| getExtraArgsMember name
            ) @ (
                package.Parents
                |> List.map ^<| getPackArgsAs names
            ) @ [
                sprintf "    member this.%sArgs = this :> %sArgs" (getAsPackName name) name
            ]
    let getArgsClassAndBuilder (param : AppParam) =
        let kind = sprintf "%sArgs" param.Name
        clearProcessedPacks ()
        [
            meta.Packs |> List.map getPackArgsMeta |> List.concat
        ]|> List.concat
        |> fun fields ->
            let argsMeta =
                fields
                |> List.map (fun f -> f :> IPropMeta)
                |> ComboMeta.Create []
            clearProcessedPacks ()
            [
                G.LooseJsonRecord (kind, [], argsMeta)
                meta.Packs |> List.map (getPackArgsMembers []) |> List.concat
                [""]
                G.ValueBuilder (kind, argsMeta)
            ] |> List.concat
    let getInterfaceHeader (param : AppParam) =
        [
            sprintf "type I%s =" param.Name
            sprintf "    inherit IPack"
        ]
    let getInterfaceFooter (param : AppParam) =
        [
            sprintf "    abstract Args : %sArgs with get" param.Name
        ]
    let getPackInherit ((name, _package) : string * PackMeta) =
        sprintf "    inherit %s" name
    let getPackAs ((name, _package) : string * PackMeta) =
        sprintf "    abstract %s : %s with get" (getAsPackName name) name
    interface IGenerator<AppParam> with
        member this.Generate param =
            [
                getAliases meta
                getArgsClassAndBuilder param
                [""]
                getInterfaceHeader param
                meta.Packs |> List.map getPackInherit
                getInterfaceFooter param
                meta.Packs |> List.map getPackAs
            ]|> List.concat

type ClassGenerator (meta : AppMeta) =
    let getClassHeader (param : AppParam) =
        [
    #if FABLE_COMPILER
            yield sprintf "type %s (logging : ILogging, scope : Scope) =" param.Name
            yield sprintf "    let env = %s logging scope" meta.Kind
    #else
            yield sprintf "type %s (loggingArgs : LoggingArgs, scope : Scope) =" param.Name
            yield sprintf "    let env = %s %s (loggingArgs.CreateLogging ()) scope" meta.Kind meta.Platform
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
        if didPackProcessed name then
            []
        else
            markPackProcessed name
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
        sprintf "        member __.%s (* %s *) : %s = %s |> Option.get" name.AsCodeMemberName packName service.Type name.AsCodeVariableName
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
    let getPackAs ((name, package) : string * PackMeta) =
        let asPackCode = getAsPackCode name
        sprintf "        member %s = %s" asPackCode asPackCode
    let rec getPackMembers (names : string list) ((name, package) : string * PackMeta) =
        if didPackProcessed name then
            []
        else
            markPackProcessed name
            let names = names @ [name]
            (
                package.Parents
                |> List.map ^<| getPackMembers names
                |> List.concat
            ) @ [
                sprintf "    interface %s with" name
                sprintf "        member this.Args = this.Args.%sArgs" <| getAsPackName name
            ] @ (
                package.Services
                |> List.map ^<| getServiceMember name
            ) @ (
                package.Spawners
                |> List.map ^<| getSpawnerMember name
                |> List.concat
            ) @ (
                package.Parents
                |> List.map getPackAs
            ) @ [
                sprintf "    member %s = this :> %s" (getAsPackCode name) name
            ]
    let getPackArgs (pack : string option) =
        pack
        |> Option.map getAsPackCode
        |> Option.map ^<| sprintf "%s "
        |> Option.defaultValue ""
    let getServiceSetter (service : ServiceMeta) (varName : string) =
        //TODO: Maybe find some better way for whether need to do cast (alias not working properly yet)
        if service.Type.StartsWith ("IAgent") then
            sprintf "            %s <- Some (%s' :> %s)" varName varName service.Type
        else
            sprintf "            %s <- Some %s'" varName varName
    let getFableServiceSetup (packName : string) (service : ServiceMeta) =
        let name = sprintf "%s%s" service.Key service.Kind
        let varName = name.AsCodeVariableName
        [
            yield sprintf "            let (* %s *) %s' = env |> Env.spawn (%s %sargs'.%s) \"%s\" \"%s\""
                packName varName service.Spec (getPackArgs service.Pack) name.AsCodeMemberName service.Kind service.Key
            yield getServiceSetter service varName
        ]
    let getFableSpawnerSetup (packName : string) (spawner : SpawnerMeta) =
        [
            sprintf "            //env |> Env.register (%s (* %s *) %sargs'.%s) \"%s\""
                spawner.Spec packName (getPackArgs spawner.Pack) spawner.Kind spawner.Kind
        ]
    let rec getFablePackSetups (names : string list) ((name, package) : string * PackMeta) =
        if didPackProcessed name then
            []
        else
            markPackProcessed name
            (
                package.Parents
                |> List.map ^<| getFablePackSetups ^<| names @ [name]
                |> List.concat
            ) @ (
                package.Services
                |> List.map ^<| getFableServiceSetup name
                |> List.concat
            ) @ (
                package.Spawners
                |> List.map ^<| getFableSpawnerSetup name
                |> List.concat
            )
    let getServiceSetup (packName : string) (service : ServiceMeta) =
        let name = sprintf "%s%s" service.Key service.Kind
        let varName = name.AsCodeVariableName
        [
            yield sprintf "            let! (* %s *) %s' = env |> Env.addServiceAsync (%s %sargs'.%s) \"%s\" \"%s\""
                packName varName service.Spec (getPackArgs service.Pack) name.AsCodeMemberName service.Kind service.Key
            yield getServiceSetter service varName
        ]
    let getSpawnerSetup (packName : string) (spawner : SpawnerMeta) =
        [
            sprintf "            do! env |> Env.registerAsync (%s (* %s *) %sargs'.%s) \"%s\""
                spawner.Spec packName (getPackArgs spawner.Pack) spawner.Kind spawner.Kind
        ]
    let rec getPackSetups (names : string list) ((name, package) : string * PackMeta) =
        if didPackProcessed name then
            []
        else
            markPackProcessed name
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
    let getExtraNews (param : AppParam) =
        [
    #if FABLE_COMPILER
            yield sprintf "    new (scope : Scope) ="
            yield sprintf "        %s (getLogging (), scope)" param.Name
    #endif
        ]
    let getFablePrivateSetupAsync (param : AppParam) =
        [
            [
                sprintf "    let setup (this : %s) : unit =" param.Name
                sprintf "        let args' = args |> Option.get"
                sprintf "        try"
            ]
            meta.Packs |> List.map (getFablePackSetups []) |> List.concat
            [
                sprintf "            this.Setup' ()"
                sprintf "            logInfo env \"%s.setup\" \"Setup_Succeed\" (E.encodeJson 4 args')" param.Name
                sprintf "        with e ->"
                sprintf "            setupError <- Some e"
                sprintf "            logException env \"%s.setup\" \"Setup_Failed\" (E.encodeJson 4 args') e" param.Name
            ]
        ]|> List.concat
    let getPrivateSetupAsync (param : AppParam) =
        [
            [
                sprintf "    let setupAsync (this : %s) : Task<unit> = task {" param.Name
                sprintf "        let args' = args |> Option.get"
                sprintf "        try"
            ]
            meta.Packs |> List.map (getPackSetups []) |> List.concat
            [
                sprintf "            do! this.SetupAsync' ()"
                sprintf "            logInfo env \"%s.setupAsync\" \"Setup_Succeed\" (E.encodeJson 4 args')" param.Name
                sprintf "        with e ->"
                sprintf "            setupError <- Some e"
                sprintf "            logException env \"%s.setupAsync\" \"Setup_Failed\" (E.encodeJson 4 args') e" param.Name
                sprintf "    }"
            ]
        ]|> List.concat
    let getPublicSetupAsync (param : AppParam) =
        [
            sprintf "    member this.Setup (callback : I%s -> unit) (getArgs : unit -> %sArgs) : I%s =" param.Name param.Name param.Name
            sprintf "        if args.IsSome then"
            sprintf "            failWith \"Already_Setup\" <| E.encodeJson 4 args.Value"
            sprintf "        else"
            sprintf "            let args' = getArgs ()"
            sprintf "            args <- Some args'"
        #if FABLE_COMPILER
            sprintf "            setup this"
            sprintf "            match setupError with"
            sprintf "            | None -> callback this.As%s" param.Name
            sprintf "            | Some e -> raise e"
        #else
            sprintf "            env.RunTask0 raiseOnFailed (fun _ -> task {"
            sprintf "                do! setupAsync this"
            sprintf "                match setupError with"
            sprintf "                | None -> callback this.As%s" param.Name
            sprintf "                | Some e -> raise e"
            sprintf "            })"
        #endif
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
        ]
    let getClassMiddle (param : AppParam) =
        [
    #if FABLE_COMPILER
            sprintf "    abstract member Setup' : unit -> unit"
            sprintf "    default __.Setup' () = ()"
    #else
            sprintf "    abstract member SetupAsync' : unit -> Task<unit>"
            sprintf "    default __.SetupAsync' () = task {"
            sprintf "        return ()"
            sprintf "    }"
    #endif
            sprintf "    member __.Args : %sArgs = args |> Option.get" param.Name
            sprintf "    interface ILogger with"
            sprintf "        member __.Log m = env.Log m"
            sprintf "    interface IPack with"
    #if !FABLE_COMPILER
            sprintf "        member __.LoggingArgs : LoggingArgs = loggingArgs"
    #endif
            sprintf "        member __.Env : IEnv = env"
        ]
    let getClassFooter (param : AppParam) =
        [
            sprintf "    interface I%s with" param.Name
            sprintf "        member this.Args : %sArgs = this.Args" param.Name
        ]
    let getClassEnd (param : AppParam) =
        [
            sprintf "    member this.As%s = this :> I%s" param.Name param.Name
        ]
    interface IGenerator<AppParam> with
        member this.Generate param =
            [
                yield getAliases meta
                yield getClassHeader param
                clearProcessedPacks ()
                yield meta.Packs |> List.map getPackFields |> List.concat
                clearProcessedPacks ()
            #if FABLE_COMPILER
                yield getFablePrivateSetupAsync param
            #else
                yield getPrivateSetupAsync param
            #endif
                yield getExtraNews param
                yield getPublicSetupAsync param
                yield getClassMiddle param
                clearProcessedPacks ()
                yield meta.Packs |> List.map (getPackMembers []) |> List.concat
                yield getClassFooter param
                clearProcessedPacks ()
                yield meta.Packs |> List.map getPackAs
                yield getClassEnd param
            ]|> List.concat

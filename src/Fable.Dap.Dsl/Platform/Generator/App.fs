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

type ArgsGenerator (meta : AppMeta) =
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
                G.LooseJsonRecord (kind, argsMeta)
                meta.Packs |> List.map (getPackArgsMembers []) |> List.concat
                [""]
                G.ValueBuilder (kind, argsMeta)
            ] |> List.concat
    interface IGenerator<AppParam> with
        member this.Generate param =
            [
                getAliases meta
                getArgsClassAndBuilder param
            ]|> List.concat


type InterfaceGenerator (meta : AppMeta) =
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
    let getServiceKind (packName : string) (service : ServiceMeta) =
        let kind = service.Kind.AsCodeMemberName
        let name = sprintf "%s%s" service.Key.AsCodeMemberName kind
        sprintf "    static member %s (* %s *) = \"%s\"" name packName kind
    let getSpawnerKind (packName : string) (spawner : SpawnerMeta) =
        let kind = spawner.Kind.AsCodeMemberName
        sprintf "    static member %s (* %s *) = \"%s\"" kind packName kind
    let rec getPackKinds (names : string list) ((name, package) : string * PackMeta) =
        if didPackProcessed name then
            []
        else
            markPackProcessed name
            let names = names @ [name]
            (
                package.Parents
                |> List.map ^<| getPackKinds names
                |> List.concat
            ) @ (
                package.Services
                |> List.map ^<| getServiceKind name
            ) @ (
                package.Spawners
                |> List.map ^<| getSpawnerKind name
            )
    let getKinds (param : AppParam) =
        clearProcessedPacks ()
        meta.Packs
        |> List.map (getPackKinds [])
        |> List.concat
        |> function
            | [] -> []
            | lines ->
                [
                    ""
                    sprintf "type %sKinds () =" param.Name
                ] @ lines
    let getServiceKey (packName : string) (service : ServiceMeta) =
        let key = service.Key.AsCodeMemberName
        let name = sprintf "%s%s" key service.Kind.AsCodeMemberName
        sprintf "    static member %s (* %s *) = \"%s\"" name packName key
    let rec getPackKeys (names : string list) ((name, package) : string * PackMeta) =
        if didPackProcessed name then
            []
        else
            markPackProcessed name
            let names = names @ [name]
            (
                package.Parents
                |> List.map ^<| getPackKeys names
                |> List.concat
            ) @ (
                package.Services
                |> List.map ^<| getServiceKey name
            )
    let getKeys (param : AppParam) =
        clearProcessedPacks ()
        meta.Packs
        |> List.map (getPackKeys [])
        |> List.concat
        |> function
            | [] -> []
            | lines ->
                [
                    ""
                    sprintf "type %sKeys () =" param.Name
                ] @ lines
    interface IGenerator<AppParam> with
        member this.Generate param =
            [
                getAliases meta
                getInterfaceHeader param
                meta.Packs |> List.map getPackInherit
                getInterfaceFooter param
                meta.Packs |> List.map getPackAs
                getKinds param
                getKeys param
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
        let name = sprintf "%s%s" service.Key.AsCodeMemberName service.Kind.AsCodeMemberName
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
        let name = sprintf "%s%s" service.Key.AsCodeMemberName service.Kind.AsCodeMemberName
        sprintf "        member __.%s (* %s *) : %s = %s |> Option.get" name packName service.Type name.AsCodeVariableName
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
    let getFableServiceSetup (param : AppParam) (packName : string) (service : ServiceMeta) =
        let kind = service.Kind.AsCodeMemberName
        let key = service.Key.AsCodeMemberName
        let name = sprintf "%s%s" key kind
        let varName = name.AsCodeVariableName
        [
            yield sprintf "            let (* %s *) %s' = env |> Env.spawn (%s %sargs'.%s) %sKinds.%s %sKeys.%s"
                packName varName service.Spec (getPackArgs service.Pack) name param.Name name param.Name name
            yield getServiceSetter service varName
        ]
    let getFableSpawnerSetup (param : AppParam) (packName : string) (spawner : SpawnerMeta) =
        let kind = spawner.Kind.AsCodeMemberName
        [
            sprintf "            //env |> Env.register (%s (* %s *) %sargs'.%s) %sKinds.%s"
                spawner.Spec packName (getPackArgs spawner.Pack) kind param.Name kind
        ]
    let rec getFablePackSetups (param : AppParam) (names : string list) ((name, package) : string * PackMeta) =
        if didPackProcessed name then
            []
        else
            markPackProcessed name
            (
                package.Parents
                |> List.map ^<| getFablePackSetups param ^<| names @ [name]
                |> List.concat
            ) @ (
                package.Services
                |> List.map ^<| getFableServiceSetup param name
                |> List.concat
            ) @ (
                package.Spawners
                |> List.map ^<| getFableSpawnerSetup param name
                |> List.concat
            )
    let getServiceSetup (param : AppParam) (packName : string) (service : ServiceMeta) =
        let kind = service.Kind.AsCodeMemberName
        let key = service.Key.AsCodeMemberName
        let name = sprintf "%s%s" key kind
        let varName = name.AsCodeVariableName
        [
            yield sprintf "            let! (* %s *) %s' = env |> Env.addServiceAsync (%s %sargs'.%s) %sKinds.%s %sKeys.%s"
                packName varName service.Spec (getPackArgs service.Pack) name param.Name name param.Name name
            yield getServiceSetter service varName
        ]
    let getSpawnerSetup (param : AppParam) (packName : string) (spawner : SpawnerMeta) =
        let kind = spawner.Kind.AsCodeMemberName
        [
            sprintf "            do! env |> Env.registerAsync (%s (* %s *) %sargs'.%s) %sKinds.%s"
                spawner.Spec packName (getPackArgs spawner.Pack) kind param.Name kind
        ]
    let rec getPackSetups (param : AppParam) (names : string list) ((name, package) : string * PackMeta) =
        if didPackProcessed name then
            []
        else
            markPackProcessed name
            (
                package.Parents
                |> List.map ^<| getPackSetups param ^<| names @ [name]
                |> List.concat
            ) @ (
                package.Services
                |> List.map ^<| getServiceSetup param name
                |> List.concat
            ) @ (
                package.Spawners
                |> List.map ^<| getSpawnerSetup param name
                |> List.concat
            )
    let getExtraNews (param : AppParam) =
        [
        #if !FABLE_COMPILER
            yield sprintf "    new (loggingArgs : LoggingArgs, scope : Scope) ="
            yield sprintf "        %s (loggingArgs.CreateLogging (), scope)" param.Name
        #endif
            yield sprintf "    new (scope : Scope) ="
            yield sprintf "        %s (getLogging (), scope)" param.Name
        ]
    let getFablePrivateSetup (param : AppParam) =
        [
            [
                sprintf "    let setup (this : %s) : unit =" param.Name
                sprintf "        let args' = args |> Option.get"
                sprintf "        try"
            ]
            meta.Packs |> List.map (getFablePackSetups param []) |> List.concat
            [
                sprintf "            this.Setup' ()"
                sprintf "            logInfo env \"%s.setup\" \"Setup_Succeed\" (E.encodeJson 4 args')" param.Name
                sprintf "        with e ->"
                sprintf "            setupError <- Some e"
                sprintf "            logException env \"%s.setup\" \"Setup_Failed\" (E.encodeJson 4 args') e" param.Name
            ]
        ]|> List.concat
    let getFablePublicSetup (param : AppParam) =
        [
            sprintf "    member this.Setup (getArgs : unit -> %sArgs) : unit =" param.Name
            sprintf "        if args.IsSome then"
            sprintf "            failWith \"Already_Setup\" <| E.encodeJson 4 args.Value"
            sprintf "        else"
            sprintf "            let args' = getArgs ()"
            sprintf "            args <- Some args'"
            sprintf "            setup this"
            sprintf "            match setupError with"
            sprintf "            | None -> ()"
            sprintf "            | Some e -> raise e"
            sprintf "    member this.Setup (args' : %sArgs) : unit =" param.Name
            sprintf "        fun () -> args'"
            sprintf "        |> this.Setup"
            sprintf "    member this.Setup (args' : Json) : unit ="
            sprintf "        fun () ->"
            sprintf "            try"
            sprintf "                castJson %sArgs.JsonDecoder args'" param.Name
            sprintf "            with e ->"
            sprintf "                logException env \"%s.Setup\" \"Decode_Failed\" args e" param.Name
            sprintf "                raise e"
            sprintf "        |> this.Setup"
            sprintf "    member this.Setup (args' : string) : unit ="
            sprintf "        let json : Json = parseJson args'"
            sprintf "        this.Setup json"
            sprintf "    member __.SetupError : exn option = setupError"
        ]
    let getPrivateSetup (param : AppParam) =
        [
            [
                sprintf "    let setupAsync (this : %s) : Task<unit> = task {" param.Name
                sprintf "        let args' = args |> Option.get"
                sprintf "        try"
            ]
            meta.Packs |> List.map (getPackSetups param []) |> List.concat
            [
                sprintf "            do! this.SetupAsync' ()"
                sprintf "            logInfo env \"%s.setupAsync\" \"Setup_Succeed\" (E.encodeJson 4 args')" param.Name
                sprintf "        with e ->"
                sprintf "            setupError <- Some e"
                sprintf "            logException env \"%s.setupAsync\" \"Setup_Failed\" (E.encodeJson 4 args') e" param.Name
                sprintf "    }"
            ]
        ]|> List.concat
    let getPublicSetup (param : AppParam) =
        [
            sprintf "    member this.SetupAsync (getArgs : unit -> %sArgs) : Task<unit> = task {" param.Name
            sprintf "        if args.IsSome then"
            sprintf "            failWith \"Already_Setup\" <| E.encodeJson 4 args.Value"
            sprintf "        else"
            sprintf "            let args' = getArgs ()"
            sprintf "            args <- Some args'"
            sprintf "            do! setupAsync this"
            sprintf "            match setupError with"
            sprintf "            | None -> ()"
            sprintf "            | Some e -> raise e"
            sprintf "        return ()"
            sprintf "        }"
            sprintf "    member this.SetupAsync (args' : %sArgs) : Task<unit> =" param.Name
            sprintf "        fun () -> args'"
            sprintf "        |> this.SetupAsync"
            sprintf "    member this.SetupAsync (args' : Json) : Task<unit> ="
            sprintf "        fun () ->"
            sprintf "            try"
            sprintf "                castJson %sArgs.JsonDecoder args'" param.Name
            sprintf "            with e ->"
            sprintf "                logException env \"%s.SetupAsync\" \"Decode_Failed\" args e" param.Name
            sprintf "                raise e"
            sprintf "        |> this.SetupAsync"
            sprintf "    member this.SetupAsync (args' : string) : Task<unit> ="
            sprintf "        let json : Json = parseJson args'"
            sprintf "        this.SetupAsync json"
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
                yield getFablePrivateSetup param
            #else
                yield getPrivateSetup param
            #endif
                yield getExtraNews param
            #if FABLE_COMPILER
                yield getFablePublicSetup param
            #else
                yield getPublicSetup param
            #endif
                yield getClassMiddle param
                clearProcessedPacks ()
                yield meta.Packs |> List.map (getPackMembers []) |> List.concat
                yield getClassFooter param
                clearProcessedPacks ()
                yield meta.Packs |> List.map getPackAs
                yield getClassEnd param
            ]|> List.concat

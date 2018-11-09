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

type private ArgsGenerator (meta : AppMeta) =
    let getArgsMeta packName (args : ArgsMeta) (name : string) =
        let key = name.AsCodeJsonKey
        match args with
        | JsonArgs name ->
            let initValue = sprintf "(%s.Default ())" name
            M.custom (name, key, initValue)
        | CodeArgs (name, code) ->
            M.coded (name, key, code)
        |> M.comment packName
    let getServiceArgsMeta packName (service : AgentMeta) =
        getArgsMeta packName service.Args <| sprintf "%s%s" service.Key service.Kind
    let getSpawnerArgsMeta packName (spawner : AgentMeta) =
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
            ) @ (
                package.Spawners
                |> List.map ^<| getSpawnerArgsMeta name
            ) @ (
                package.ExtraArgs
                |> List.map ^<| getExtraArgsMeta name
            )
    let getScopeMeta (param : AppParam) =
        M.custom ("Scope", "scope", "NoScope")
        |> M.comment ^<| sprintf "%sArgs" param.Name
    let getSetupMeta (param : AppParam) =
        M.coded (sprintf "I%s -> unit" param.Name, "Setup", "ignore")
        |> M.comment ^<| sprintf "%sArgs" param.Name
    let getServiceArgsMember (packName : string) (service : AgentMeta) =
        let name = sprintf "%s%s" service.Key service.Kind
        let name = name.AsCodeMemberName
        sprintf "        member this.%s (* %s *) : %s = this.%s" (getArgsMemberName name) packName service.Args.Type name
    let getSpawnerArgsMember (packName : string) (spawner : AgentMeta) =
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
                (getScopeMeta param) :: (getSetupMeta param) :: fields
                |> ComboMeta.Create []
            clearProcessedPacks ()
            [
                RecordParam.Create kind true true
                |> fun p -> {p with InGroup = true}
                |> generate (new Combo.RecordGenerator (argsMeta))
                meta.Packs |> List.map (getPackArgsMembers []) |> List.concat
                [""]
                G.ValueBuilder (kind, argsMeta)
            ] |> List.concat
    member __.Generate (param : AppParam) =
        [
            getAliases meta
            getArgsClassAndBuilder param
        ]|> List.concat


type InterfaceGenerator (meta : AppMeta) =
    let getInterfaceHeader (param : AppParam) =
        [
            sprintf ""
            sprintf "type I%s =" param.Name
            sprintf "    inherit IApp<I%s>" param.Name
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
    let getServiceKind (packName : string) (service : AgentMeta) =
        let kind = service.Kind.AsCodeMemberName
        let name = sprintf "%s%s" service.Key.AsCodeMemberName kind
        sprintf "    static member %s (* %s *) = \"%s\"" name packName kind
    let getSpawnerKind (packName : string) (spawner : AgentMeta) =
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
    let getServiceKey (packName : string) (service : AgentMeta) =
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
                getKinds param
                getKeys param
                getInterfaceHeader param
                meta.Packs |> List.map getPackInherit
                getInterfaceFooter param
                meta.Packs |> List.map getPackAs
                [""]
                (new ArgsGenerator (meta)) .Generate param
            ]|> List.concat

type ClassGenerator (meta : AppMeta) =
    let getClassHeader (param : AppParam) =
        [
    #if FABLE_COMPILER
            yield sprintf "type %s (logging : ILogging, args : %sArgs) =" param.Name param.Name
            yield sprintf "    let env = Env.create logging args.Scope (%s ())" meta.Clock
    #else
            yield sprintf "type %s (param : EnvParam, args : %sArgs) =" param.Name param.Name
            yield sprintf "    let env = Env.create param"
    #endif
            yield sprintf "    let mutable setupResult : Result<bool, exn> option = None"
        ]
    let getServiceField (packName : string) (service : AgentMeta) =
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
    let getServiceMember (packName : string) (service : AgentMeta) =
        let name = sprintf "%s%s" service.Key.AsCodeMemberName service.Kind.AsCodeMemberName
        sprintf "        member __.%s (* %s *) : %s = %s |> Option.get" name packName service.Type name.AsCodeVariableName
    let getSpawnerMember (packName : string) (spawner : AgentMeta) =
        let kind = spawner.Kind.AsCodeMemberName
        [
            sprintf "        member __.Get%sAsync (key : Key) (* %s *) : Task<%s * bool> = task {" kind packName spawner.Type
            sprintf "            let! (agent, isNew) = env.HandleAsync <| DoGetAgent \"%s\" key" kind
            sprintf "            return (agent :?> %s, isNew)" spawner.Type
            sprintf "        }"
        ]
    let getPackAs ((name, package) : string * PackMeta) =
        sprintf "        member this.%s = this.%s" (getAsPackName name) (getAsPackName name)
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
                yield sprintf "    interface %s with" name
            #if !FABLE_COMPILER
                yield sprintf "        member this.Args = this.Args.%sArgs" <| getAsPackName name
            #endif
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
                sprintf "    member this.%s = this :> %s" (getAsPackName name) name
            ]
    let getPackArgs (pack : string option) =
        pack
        |> Option.map ^<| (fun name -> sprintf "this.%s " <| getAsPackName name)
        |> Option.defaultValue ""
    let getServiceSetter (service : AgentMeta) (varName : string) =
        //TODO: Maybe find some better way for whether need to do cast (alias not working properly yet)
        if service.Type.StartsWith ("IAgent") then
            sprintf "            %s <- Some (%s' :> %s)" varName varName service.Type
        else
            sprintf "            %s <- Some %s'" varName varName
    let getFableServiceSetup (param : AppParam) (packName : string) (service : AgentMeta) =
        let kind = service.Kind.AsCodeMemberName
        let key = service.Key.AsCodeMemberName
        let name = sprintf "%s%s" key kind
        let varName = name.AsCodeVariableName
        [
            yield sprintf "            let (* %s *) %s' = env |> Env.spawn (%s %sargs.%s) %sKinds.%s %sKeys.%s"
                packName varName service.Spec (getPackArgs service.Pack) name param.Name name param.Name name
            yield getServiceSetter service varName
        ]
    let getFableSpawnerSetup (param : AppParam) (packName : string) (spawner : AgentMeta) =
        let kind = spawner.Kind.AsCodeMemberName
        [
            sprintf "            //env |> Env.register (%s (* %s *) %sargs.%s) %sKinds.%s"
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
    let getServiceSetup (param : AppParam) (packName : string) (service : AgentMeta) =
        let kind = service.Kind.AsCodeMemberName
        let key = service.Key.AsCodeMemberName
        let name = sprintf "%s%s" key kind
        let varName = name.AsCodeVariableName
        [
            yield sprintf "            let! (* %s *) %s' = env |> Env.addServiceAsync (%s %sargs.%s) %sKinds.%s %sKeys.%s"
                packName varName service.Spec (getPackArgs service.Pack) name param.Name name param.Name name
            yield getServiceSetter service varName
        ]
    let getSpawnerSetup (param : AppParam) (packName : string) (spawner : AgentMeta) =
        let kind = spawner.Kind.AsCodeMemberName
        [
            sprintf "            do! env |> Env.registerAsync (%s (* %s *) %sargs.%s) %sKinds.%s"
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
            yield sprintf "    new (logging : ILogging, a : %sArgs) =" param.Name
            if meta.Platform.IsSome then
                yield sprintf "        let platform = new %s (logging)" meta.Platform.Value
            else
                yield sprintf "        let platform = Feature.create<IPlatform> logging"
            yield sprintf "        let clock = new %s ()" meta.Clock
            yield sprintf "        %s (Env.param platform logging a.Scope clock, a)" param.Name
            yield sprintf "    new (loggingArgs : LoggingArgs, a : %sArgs) =" param.Name
            yield sprintf "        %s (Feature.createLogging loggingArgs, a)" param.Name
            yield sprintf "    new (a : %sArgs) =" param.Name
            yield sprintf "        %s (getLogging (), a)" param.Name
        #endif
        ]
    let getFableSetup (param : AppParam) =
        [
            [
                sprintf "    member this.Setup () : unit ="
                sprintf "        if setupResult.IsSome then"
                sprintf "           failWith \"Already_Setup\" setupResult.Value"
                sprintf "        try"
                sprintf "            setupResult <- Some (Ok false)"
            ]
            meta.Packs |> List.map (getFablePackSetups param []) |> List.concat
            [
                sprintf "            this.Setup' ()"
                sprintf "            logInfo env \"%s.setup\" \"Setup_Succeed\" (encodeJson 4 args)" param.Name
                sprintf "            args.Setup this.As%s" param.Name
                sprintf "            setupResult <- Some (Ok true)"
                sprintf "        with e ->"
                sprintf "            setupResult <- Some (Error e)"
                sprintf "            logException env \"%s.setup\" \"Setup_Failed\" (encodeJson 4 args) e" param.Name
            ]
        ]|> List.concat
    let getSetup (param : AppParam) =
        [
            [
                sprintf "    member this.SetupAsync () : Task<unit> = task {"
                sprintf "        if setupResult.IsSome then"
                sprintf "           failWith \"Already_Setup\" setupResult.Value"
                sprintf "        try"
                sprintf "            setupResult <- Some (Ok false)"
            ]
            meta.Packs |> List.map (getPackSetups param []) |> List.concat
            [
                sprintf "            do! this.SetupAsync' ()"
                sprintf "            logInfo env \"%s.setupAsync\" \"Setup_Succeed\" (encodeJson 4 args)" param.Name
                sprintf "            args.Setup this.As%s" param.Name
                sprintf "            setupResult <- Some (Ok true)"
                sprintf "        with e ->"
                sprintf "            setupResult <- Some (Error e)"
                sprintf "            logException env \"%s.setupAsync\" \"Setup_Failed\" (encodeJson 4 args) e" param.Name
                sprintf "            raise e"
                sprintf "    }"
            ]
        ]|> List.concat
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
            sprintf "    member __.Args : %sArgs = args" param.Name
            sprintf "    member __.Env : IEnv = env"
            sprintf "    member __.SetupResult : Result<bool, exn> option = setupResult"
        #if FABLE_COMPILER
            sprintf "    interface INeedSetup with"
            sprintf "       member this.SetupResult = this.SetupResult"
            sprintf "        member this.Setup () = this.Setup ()"
        #else
            sprintf "    interface INetSetupAsync with"
            sprintf "       member this.SetupResult = this.SetupResult"
            sprintf "        member this.SetupAsync () = this.SetupAsync ()"
        #endif
            sprintf "    interface IRunner<I%s> with" param.Name
            sprintf "        member this.Runner = this.As%s" param.Name
        #if !FABLE_COMPILER
            sprintf "        member this.RunFunc func = runFunc' this func"
            sprintf "        member this.AddTask onFailed getTask = addTask' this onFailed getTask"
            sprintf "        member this.RunTask onFailed getTask = runTask' this onFailed getTask"
        #endif
            sprintf "    interface IRunner with"
            sprintf "        member __.Clock = env.Clock"
        #if !FABLE_COMPILER
            sprintf "        member __.Console0 = env.Console0"
            sprintf "        member this.RunFunc0 func = runFunc' this func"
            sprintf "        member this.AddTask0 onFailed getTask = addTask' this onFailed getTask"
            sprintf "        member this.RunTask0 onFailed getTask = runTask' this onFailed getTask"
            sprintf "    interface ITaskManager with"
            sprintf "        member __.StartTask task = env.StartTask task"
            sprintf "        member __.ScheduleTask task = env.ScheduleTask task"
            sprintf "        member __.PendingTasksCount = env.PendingTasksCount"
            sprintf "        member __.StartPendingTasks () = env.StartPendingTasks ()"
            sprintf "        member __.ClearPendingTasks () = env.ClearPendingTasks ()"
            sprintf "        member __.RunningTasksCount = env.RunningTasksCount"
            sprintf "        member __.CancelRunningTasks () = env.CancelRunningTasks ()"
        #endif
            sprintf "    interface IPack with"
            sprintf "        member __.Env : IEnv = env"
            sprintf "    interface ILogger with"
            sprintf "        member __.Log m = env.Log m"
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
                yield getExtraNews param
            #if FABLE_COMPILER
                yield getFableSetup param
            #else
                yield getSetup param
            #endif
                yield getClassMiddle param
                clearProcessedPacks ()
                yield meta.Packs |> List.map (getPackMembers []) |> List.concat
                yield getClassFooter param
                clearProcessedPacks ()
                yield meta.Packs |> List.map getPackAs
                yield getClassEnd param
            ]|> List.concat

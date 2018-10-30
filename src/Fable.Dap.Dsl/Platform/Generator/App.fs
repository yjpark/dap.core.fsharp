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
    let getGuiInterface (param : AppParam) =
        [
            sprintf "    abstract GuiContext : SynchronizationContext with get"
            sprintf "    abstract GetGuiTask : GetTask<I%s, 'res> -> Task<'res>" param.Name
            sprintf "    abstract RunGuiTask : OnFailed<I%s> -> GetTask<I%s, unit> -> unit" param.Name param.Name
            sprintf "    abstract RunGuiFunc : Func<I%s, unit> -> unit" param.Name
        ]
    interface IGenerator<AppParam> with
        member this.Generate param =
            let extra =
            #if FABLE_COMPILER
                []
            #else
                if param.IsGui then
                    getGuiInterface param
                else
                    []
            #endif
            [
                getAliases meta
                getKinds param
                getKeys param
                getInterfaceHeader param
                meta.Packs |> List.map getPackInherit
                getInterfaceFooter param
                meta.Packs |> List.map getPackAs
                extra
                [""]
                (new ArgsGenerator (meta)) .Generate param
            ]|> List.concat

type ClassGenerator (meta : AppMeta) =
    let getClassHeader (param : AppParam) =
        [
            yield sprintf "type %s (logging : ILogging, args : %sArgs) as this =" param.Name param.Name
    #if FABLE_COMPILER
            yield sprintf "    let env = %s logging args.Scope" meta.Kind
    #else
            yield sprintf "    let env = %s %s logging args.Scope" meta.Kind meta.Platform
    #endif
            yield sprintf "    let mutable setupError : exn option = None"
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
        sprintf "        member __.%s = this.%s" (getAsPackName name) (getAsPackName name)
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
                yield sprintf "        member __.Args = this.Args.%sArgs" <| getAsPackName name
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
                sprintf "    member __.%s = this :> %s" (getAsPackName name) name
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
            yield sprintf "    new (loggingArgs : LoggingArgs, a : %sArgs) = new %s (loggingArgs.CreateLogging (), a)" param.Name param.Name
            yield sprintf "    new (a : %sArgs) = new %s (getLogging (), a)" param.Name param.Name
        #endif
        ]
    let getFablePrivateSetup (param : AppParam) =
        [
            [
                sprintf "    let setup () : unit ="
                sprintf "        try"
            ]
            meta.Packs |> List.map (getFablePackSetups param []) |> List.concat
            [
                sprintf "            this.Setup' ()"
                sprintf "            logInfo env \"%s.setup\" \"Setup_Succeed\" (encodeJson 4 args)" param.Name
                sprintf "            args.Setup this.As%s" param.Name
                sprintf "        with e ->"
                sprintf "            setupError <- Some e"
                sprintf "            logException env \"%s.setup\" \"Setup_Failed\" (encodeJson 4 args) e" param.Name
                sprintf "    do ("
                sprintf "        setup ()"
                sprintf "    )"
            ]
        ]|> List.concat
    let getPrivateSetup (param : AppParam) =
        [
            [
                sprintf "    let setupAsync (_runner : IRunner) : Task<unit> = task {"
                sprintf "        try"
            ]
            meta.Packs |> List.map (getPackSetups param []) |> List.concat
            [
                sprintf "            do! this.SetupAsync' ()"
                sprintf "            logInfo env \"%s.setupAsync\" \"Setup_Succeed\" (encodeJson 4 args)" param.Name
                sprintf "            args.Setup this.As%s" param.Name
                sprintf "        with e ->"
                sprintf "            setupError <- Some e"
                sprintf "            logException env \"%s.setupAsync\" \"Setup_Failed\" (encodeJson 4 args) e" param.Name
                sprintf "            raise e"
                sprintf "    }"
                sprintf "    do ("
                sprintf "        env.RunTask0 raiseOnFailed setupAsync"
                sprintf "    )"
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
            sprintf "    member __.SetupError : exn option = setupError"
            sprintf "    interface ILogger with"
            sprintf "        member __.Log m = env.Log m"
            sprintf "    interface IRunner<I%s> with" param.Name
            sprintf "        member __.Runner = this.As%s" param.Name
            sprintf "        member __.RunFunc func = runFunc' this func"
            sprintf "        member __.AddTask onFailed getTask = addTask' this onFailed getTask"
            sprintf "        member __.RunTask onFailed getTask = runTask' this onFailed getTask"
        #if !FABLE_COMPILER
        #endif
            sprintf "    interface IRunner with"
            sprintf "        member __.Clock = env.Clock"
        #if !FABLE_COMPILER
            sprintf "        member __.Console0 = env.Console0"
            sprintf "        member __.RunFunc0 func = runFunc' this func"
            sprintf "        member __.AddTask0 onFailed getTask = addTask' this onFailed getTask"
            sprintf "        member __.RunTask0 onFailed getTask = runTask' this onFailed getTask"
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
        ]
    let getClassFooter (param : AppParam) =
        [
            sprintf "    interface I%s with" param.Name
            sprintf "        member __.Args : %sArgs = this.Args" param.Name
        ]
    let getClassEnd (param : AppParam) =
        [
            sprintf "    member __.As%s = this :> I%s" param.Name param.Name
        ]
    let getGuiFields (param : AppParam) =
        [
            "    let mutable guiContext : SynchronizationContext option = None"
        ]
    let getGuiSetup (param : AppParam) =
        (*
         * Don't know why but if set guiContext with SynchronizationContext.Current
         * here in the base class, it's always null, even though the thread is actual
         * the proper one, guess it's some timing issue.
         * (under GtkSharp, not 100% sure with other platform)
         * Current solution is to call `base.SetupGuiContext' ()` in sub classes' do
         * block, which can get proper value.
         *)
        [
            "    member __.SetupGuiContext' () ="
            "        match guiContext with"
            "        | Some guiContext' ->"
            "            failWith \"GuiContext_Already_Setup\" guiContext'"
            "        | None ->"
            "            let guiContext' = SynchronizationContext.Current"
            "            if guiContext' =? null then"
            "                logError env \"SetupGuiContext'\" \"Failed\" guiContext'"
            "            else"
            "                guiContext <- Some guiContext'"
            "                logInfo env \"SetupGuiContext'\" \"Succeed\" guiContext'"
        ]
    let getGuiMembers (param : AppParam) =
        [
            sprintf "        member __.GuiContext ="
            sprintf "            match guiContext with"
            sprintf "            | Some guiContext' -> guiContext'"
            sprintf "            | None -> failWith \"GuiContext_Not_Setup\" this"
            sprintf "        member __.GetGuiTask (getTask : GetTask<I%s, 'res>) : Task<'res> = task {" param.Name
            sprintf "            return! async {"
            sprintf "                do! Async.SwitchToContext (this.AsApp.GuiContext)"
            sprintf "                return! Async.AwaitTask (getTask this)"
            sprintf "            }"
            sprintf "        }"
            sprintf "        member __.RunGuiTask (onFailed : OnFailed<I%s>) (getTask : GetTask<I%s, unit>) : unit =" param.Name param.Name
            sprintf "            (this :> IRunner<IApp>).RunTask onFailed (fun _ -> this.AsApp.GetGuiTask getTask)"
            sprintf "        member __.RunGuiFunc (func : Func<I%s, unit>) : unit =" param.Name
            sprintf "            this.As%s.RunGuiTask ignoreOnFailed (fun _ -> task {" param.Name
            sprintf "                runFunc' this func |> ignore"
            sprintf "            })"
        ]
    interface IGenerator<AppParam> with
        member this.Generate param =
            [
                yield getAliases meta
                yield getClassHeader param
                clearProcessedPacks ()
                yield meta.Packs |> List.map getPackFields |> List.concat
                clearProcessedPacks ()
            #if !FABLE_COMPILER
                if param.IsGui then
                    yield getGuiFields param
            #endif
            #if FABLE_COMPILER
                yield getFablePrivateSetup param
            #else
                yield getPrivateSetup param
            #endif
                yield getExtraNews param
            #if !FABLE_COMPILER
                if param.IsGui then
                    yield getGuiSetup param
            #endif
                yield getClassMiddle param
                clearProcessedPacks ()
                yield meta.Packs |> List.map (getPackMembers []) |> List.concat
                yield getClassFooter param
                clearProcessedPacks ()
                yield meta.Packs |> List.map getPackAs
            #if !FABLE_COMPILER
                if param.IsGui then
                    yield getGuiMembers param
            #endif

                yield getClassEnd param
            ]|> List.concat

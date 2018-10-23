[<AutoOpen>]
module Dap.Platform.Meta.Helper

open Microsoft.FSharp.Quotations

open Dap.Prelude
open Dap.Context
open Dap.Context.Meta
open Dap.Context.Meta.Util
open Dap.Context.Generator.Util
open Dap.Platform

let pack (parents : Expr<PackMeta> list) = new Pack.Builder (parents)
let live = new App.Builder ("Env.live", "MailboxPlatform")

let jsonInitValue (type' : string) (encoder : JsonEncoder<'args>) (args : 'args) =
#if FABLE_COMPILER
    // this can't be called in Fable generators, since encoder calls js code
    failWith "Fable:jsonInitValue" <| sprintf "%s -> %s" type' ^<| args.ToString ()
#else
    let json = E.encode 0 <| encoder args
    if json.StartsWith "\"" then
        sprintf "(decodeJsonString %s.JsonDecoder \"\"%s\"\")" type' json
    else
        sprintf "(decodeJsonValue %s.JsonDecoder \"\"\"%s\"\"\")" type' json
#endif

let jsonCodeArgs (type' : string) (encoder : JsonEncoder<'args>) (args : 'args) =
    args
    |> jsonInitValue type' encoder
    |> CodeArgs type'

type M with
    static member noArgs = CodeArgs "NoArgs" "NoArgs"

type M with
    static member service (aliases : ModuleAlias list, args : ArgsMeta, type' : string, spec : string, kind : Kind, ?key : Key) =
        let key = defaultArg key NoKey
        ServiceMeta.Create aliases args type' spec None kind key

type M with
    static member spawner (aliases : ModuleAlias list, args : ArgsMeta, type' : string, spec : string, kind : Kind) =
        SpawnerMeta.Create aliases args type' spec None kind

type M with
    static member jsonArgs (aliases : ModuleAlias list, name : string, ?key : Key) =
        let key = defaultArg key name.AsCodeVariableName
        ExtraArgsMeta.Create aliases (JsonArgs name) key
    static member jsonArgs (aliases : ModuleAlias list, expr, ?key : Key) =
        let (name, _meta) = unquotePropertyGetExpr expr
        M.jsonArgs (aliases, name, ?key = key)

type M with
    static member codeArgs (aliases : ModuleAlias list, name : string, code : string, key : Key) =
        ExtraArgsMeta.Create aliases (CodeArgs name code) key

    static member codeArgs (aliases : ModuleAlias list, expr : Expr<string>, key : Key) =
        let (name, code) = unquotePropertyGetExpr expr
        M.codeArgs (aliases, name, code, key)

type M with
    static member stateSpawner (aliases : ModuleAlias list, name : string, spawner : string, kind : Kind) =
        let alias = "State", "Dap.Platform.State"
        let args = CodeArgs (sprintf "State.Args<%s>" name) spawner
        let type' = sprintf "State.Agent<%s>" name
        let spec = "Dap.Platform.State.spec"
        M.spawner (alias :: aliases, args, type', spec, kind)
    static member stateSpawner (aliases : ModuleAlias list, expr : Expr<string>, kind : Kind) =
        let (name, spawner) = unquotePropertyGetExpr expr
        M.stateSpawner (aliases, name, spawner, kind)
    static member stateService (aliases : ModuleAlias list, name : string, spawner : string, kind : Kind, ?key : Key) =
        let key = defaultArg key NoKey
        M.stateSpawner (aliases, name, spawner, kind)
        |> fun s -> s.ToService key
    static member stateService (aliases : ModuleAlias list, expr : Expr<string>, kind : Kind, ?key : Key) =
        let key = defaultArg key NoKey
        M.stateSpawner (aliases, expr, kind)
        |> fun s -> s.ToService key

type M with
    static member contextSpawner (aliases : ModuleAlias list, name : string, spawner : string, kind : Kind) =
        let alias = "Context", "Dap.Platform.Context"
        let args = CodeArgs (sprintf "Context.Args<%s>" name) spawner
        let type' = sprintf "Context.Agent<%s>" name
        let spec = "Dap.Platform.Context.spec"
        M.spawner (alias :: aliases, args, type', spec, kind)
    static member contextSpawner (aliases : ModuleAlias list, expr : Expr<string>, kind : Kind) =
        let (name, spawner) = unquotePropertyGetExpr expr
        M.contextSpawner (aliases, name, spawner, kind)
    static member contextService (aliases : ModuleAlias list, name : string, spawner : string, kind : Kind, ?key : Key) =
        let key = defaultArg key NoKey
        M.contextSpawner (aliases, name, spawner, kind)
        |> fun s -> s.ToService key
    static member contextService (aliases : ModuleAlias list, expr : Expr<string>, kind : Kind, ?key : Key) =
        let key = defaultArg key NoKey
        M.contextSpawner (aliases, expr, kind)
        |> fun s -> s.ToService key

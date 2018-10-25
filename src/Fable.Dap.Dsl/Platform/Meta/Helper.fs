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
    static member agent (args : ArgsMeta, type' : string, spec : string, kind : Kind, ?key : Key, ?aliases : ModuleAlias list) =
        let key = defaultArg key NoKey
        let aliases = defaultArg aliases []
        AgentMeta.Create aliases args type' spec None kind key

type M with
    static member jsonArgs (name : string, ?key : Key, ?aliases : ModuleAlias list) =
        let key = defaultArg key name.AsCodeVariableName
        let aliases = defaultArg aliases []
        ExtraArgsMeta.Create aliases (JsonArgs name) key
    static member jsonArgs (expr, ?key : Key, ?aliases : ModuleAlias list) =
        let (name, _meta) = unquotePropertyGetExpr expr
        M.jsonArgs (name, ?key = key, ?aliases = aliases)

type M with
    static member codeArgs (name : string, code : string, key : Key, ?aliases : ModuleAlias list) =
        let aliases = defaultArg aliases []
        ExtraArgsMeta.Create aliases (CodeArgs name code) key

    static member codeArgs (expr : Expr<string>, key : Key, ?aliases : ModuleAlias list) =
        let (name, code) = unquotePropertyGetExpr expr
        M.codeArgs (name, code, key, ?aliases = aliases)

type M with
    static member state (name : string, ?spawner : string, ?kind : Kind, ?key : Key, ?aliases : ModuleAlias list) =
        let spawner = defaultArg spawner <| sprintf "%s.AddToAgent" name.AsCodeClassName
        let kind = defaultArg kind name.AsCodeClassName
        let aliases = defaultArg aliases []
        let alias = "State", "Dap.Platform.State"
        let args = CodeArgs (sprintf "State.Args<%s>" name) spawner
        let type' = sprintf "State.Agent<%s>" name
        let spec = "Dap.Platform.State.spec"
        M.agent (args, type', spec, kind, ?key = key, aliases = alias :: aliases)

type M with
    static member context (name : string, ?spawner : string, ?kind : Kind, ?key : Key, ?aliases : ModuleAlias list) =
        let spawner = defaultArg spawner <| sprintf "%s.AddToAgent" name.AsCodeClassName
        let kind = defaultArg kind name.AsCodeClassName
        let aliases = defaultArg aliases []
        let alias = "Context", "Dap.Platform.Context"
        let args = CodeArgs (sprintf "Context.Args<%s>" name) spawner
        let type' = sprintf "Context.Agent<%s>" name
        let spec = "Dap.Platform.Context.spec"
        M.agent (args, type', spec, kind, ?key = key, aliases = alias :: aliases)
    static member context (expr : Expr<ContextMeta>, ?kind : Kind, ?key : Key, ?aliases : ModuleAlias list) =
        let (name, meta) = unquotePropertyGetExpr expr
        M.context (name.AsCodeInterfaceName, ?kind = kind, ?key = key, ?aliases = aliases)
[<AutoOpen>]
module Dap.Context.Generator.Helper

open Microsoft.FSharp.Quotations

open Dap.Prelude
open Dap.Context
open Dap.Context.Meta
open Dap.Context.Meta.Util
open Dap.Context.Generator.Util

let private getHeader (keepCommentMode : bool) (param : IParam) =
    []
    |> List.append (
        [
            sprintf "(*"
            sprintf " * Generated: <%s>" param.Category
        ] @ (if param.Desc <> "" then [sprintf " *     %s" param.Desc] else [])
    ) |> List.extend (if keepCommentMode then [] else [" *)"])

let generate<'param when 'param :> IParam> (generator : IGenerator<'param>) (param : 'param) =
    generator.Generate param
    |> List.append ^<| getHeader false param

let concatSections (sections : Lines list) =
    sections
    |> List.map (fun section ->
        "" :: section
    )|> List.concat

type internal ModuleGenerator (sections : Lines list) =
    member __.Generate (param : ModuleParam) : Lines =
        concatSections sections
        |> standardizeModuleLines
        |> List.append [
            if param.AutoOpen then
                yield sprintf "[<AutoOpen>]"
            if param.RequireQualifiedAccess then
                yield sprintf "[<RequireQualifiedAccess>]"
            yield sprintf "module %s" param.Name
            yield ""
        ]

let generateModule sections param =
    ModuleGenerator(sections) .Generate param

let writeCodeFile (path : string) (lines : Lines) =
    let content = lines |> String.concat "\n"
    System.IO.File.WriteAllText (path, content)
    sprintf "Code File Generated: %s -> %d Lines" path lines.Length

let ensureOpens (sections : Lines list) (opens : Lines) =
    opens
    |> List.filter (fun open' ->
        sections
        |> List.exists (fun section ->
            section
            |> List.exists (fun line -> line = open')
        )|> not
    )|> function
        | [] -> sections
        | opens -> opens :: sections

type G = CodeGeneratorHelper with
    static member File (path, lines) =
        writeCodeFile path lines
    static member File (folder, filename, lines) =
        let path = System.IO.Path.Combine (folder, filename)
        G.File (path, lines)
    static member File (folder, subFolder, filename, lines) =
        let path = System.IO.Path.Combine (folder, subFolder, filename)
        G.File (path, lines)
    static member File (segments : string list, lines) =
        let path = System.IO.Path.Combine (segments |> List.toArray)
        G.File (path, lines)
    static member File (segments1 : string list, segments2 : string list, lines) =
        G.File (segments1 @ segments2, lines)

type G with
    static member Module (name, autoOpen, requireQualifiedAccess, sections) =
        let sections =
            [
                "open Dap.Prelude"
                "open Dap.Context"
            ] |> ensureOpens sections
        ModuleParam.Create name autoOpen requireQualifiedAccess
        |> generateModule sections
    static member Module (name, sections) =
        G.Module (name, false, false, sections)
    static member AutoOpenModule (name, sections : Lines list) =
        G.Module (name, true, false, sections)
    static member QualifiedModule (name, sections : Lines list) =
        G.Module (name, false, true, sections)
    static member AutoOpenQualifiedModule (name, sections : Lines list) =
        G.Module (name, true, true, sections)
    static member BuilderModule (name : string, sections) =
        let autoOpen = not <| name.EndsWith ("Builder")
        let sections =
            [
                "open Dap.Prelude"
                "open Dap.Context"
                "open Dap.Context.Builder"
            ] |> ensureOpens sections
        ModuleParam.Create name autoOpen false
        |> generateModule sections

type G with
    static member ComboInterface (name, meta : ComboMeta, ?parents : string list) =
        let parents = defaultArg parents []
        InterfaceParam.Create InterfaceType.ComboInterface name parents
        |> generate (new Combo.InterfaceGenerator (meta))
    static member ComboInterface (expr : Expr<ComboMeta>, ?parents : string list) =
        let (name, meta) = unquotePropertyGetExpr expr
        G.ComboInterface (name, meta, ?parents = parents)
    static member ValueInterface (name, meta, ?parents : string list) =
        let parents = defaultArg parents []
        InterfaceParam.Create InterfaceType.ValueInterface name parents
        |> generate (new Combo.InterfaceGenerator (meta))
    static member ValueInterface (expr : Expr<ComboMeta>, ?parents : string list) =
        let (name, meta) = unquotePropertyGetExpr expr
        G.ValueInterface (name, meta, ?parents = parents)

type G with
    static member Record (name, meta : ComboMeta, ?isJson : bool, ?isLoose : bool) =
        let isJson = defaultArg isJson false
        let isLoose = defaultArg isLoose false
        RecordParam.Create name isJson isLoose
        |> generate (new Combo.RecordGenerator (meta))
    static member Record (expr : Expr<ComboMeta>, ?isJson : bool, ?isLoose : bool) =
        let (name, meta) = unquotePropertyGetExpr expr
        G.Record (name, meta, ?isJson = isJson, ?isLoose = isLoose)
    static member JsonRecord (name, meta : ComboMeta) =
        G.Record (name, meta, isJson = true)
    static member JsonRecord (expr : Expr<ComboMeta>) =
        G.Record (expr, isJson = true)
    static member LooseJsonRecord (name, meta : ComboMeta) =
        G.Record (name, meta, isJson = true, isLoose = true)
    static member LooseJsonRecord (expr : Expr<ComboMeta>) =
        G.Record (expr, isJson = true, isLoose = true)

type G with
    static member Union (name, meta : UnionMeta, ?isJson : bool) =
        let isJson = defaultArg isJson false
        UnionParam.Create name isJson
        |> generate (new Union.UnionGenerator (meta))
    static member Union (expr : Expr<UnionMeta>, ?isJson : bool) =
        let (name, meta) = unquotePropertyGetExpr expr
        G.Union (name, meta, ?isJson = isJson)
    static member JsonUnion (name, meta : UnionMeta) =
        G.Union (name, meta, isJson = true)
    static member JsonUnion (expr : Expr<UnionMeta>) =
        G.Union (expr, isJson = true)

type G with
    static member Combo (name, meta : ComboMeta, ?isAbstract : bool, ?isFinal : bool) =
        let isAbstract = defaultArg isAbstract false
        let isFinal = defaultArg isFinal false
        ComboParam.Create name isAbstract isFinal
        |> generate (new Combo.ClassGenerator (meta))
    static member Combo (expr : Expr<ComboMeta>, ?isAbstract : bool, ?isFinal : bool) =
        let (name, meta) = unquotePropertyGetExpr expr
        G.Combo (name, meta, ?isAbstract = isAbstract, ?isFinal = isFinal)
    static member AbstractCombo (name, meta : ComboMeta) =
        G.Combo (name, meta, isAbstract = true)
    static member AbstractCombo (expr : Expr<ComboMeta>) =
        G.Combo (expr, isAbstract = true)
    static member FinalCombo (name, meta : ComboMeta) =
        G.Combo (name, meta, isFinal = true)
    static member FinalCombo (expr : Expr<ComboMeta>) =
        G.Combo (expr, isFinal = true)

type G with
    static member ComboBuilder (name, kind, key, meta) =
        BuilderParam.Create ComboBuilder name kind key
        |> generate (new Combo.BuilderGenerator (meta))
    static member ComboBuilder (kind : string, key, meta) =
        let name = sprintf "%sBuilder" kind.AsCodeMemberName
        G.ComboBuilder (name, kind, key, meta)
    static member ComboBuilder (expr, key : string) =
        let (kind, meta) = unquotePropertyGetExpr expr
        G.ComboBuilder (kind, key, meta)
    static member ComboBuilder (expr) =
        let (kind, meta) = unquotePropertyGetExpr expr
        let key = kind.AsCodeJsonKey
        G.ComboBuilder (kind, key, meta)

type G with
    static member ValueBuilder (name, kind, key, meta) =
        BuilderParam.Create ValueBuilder name kind key
        |> generate (new Combo.BuilderGenerator (meta))
    static member ValueBuilder (kind : string, key, meta) =
        let name = sprintf "%sBuilder" kind.AsCodeMemberName
        G.ValueBuilder (name, kind, key, meta)
    static member ValueBuilder (kind : string, meta) =
        let key = kind.AsCodeJsonKey
        G.ValueBuilder (kind, key, meta)
    static member ValueBuilder (expr, key : string) =
        let (kind, meta) = unquotePropertyGetExpr expr
        G.ValueBuilder (kind, key, meta)
    static member ValueBuilder (expr) =
        let (kind, meta) = unquotePropertyGetExpr expr
        let key = kind.AsCodeJsonKey
        G.ValueBuilder (kind, key, meta)

type G with
    static member ContextInterface (name, meta : ContextMeta) =
        ContextParam.Create name
        |> generate (new Context.InterfaceGenerator (meta))
    static member ContextInterface (expr : Expr<ContextMeta>) =
        let (name, meta) = unquotePropertyGetExpr expr
        G.ContextInterface (name, meta)

type G with
    static member ContextClass (name, meta : ContextMeta) =
        ContextParam.Create name
        |> generate (new Context.ClassGenerator (meta))
    static member ContextClass (expr : Expr<ContextMeta>) =
        let (name, meta) = unquotePropertyGetExpr expr
        G.ContextClass (name, meta)

type G with
    static member Context (name, meta : ContextMeta) =
        [
            G.ContextInterface (name, meta)
            [""]
            G.ContextClass (name, meta)
        ]|> List.concat
    static member Context (expr : Expr<ContextMeta>) =
        let (name, meta) = unquotePropertyGetExpr expr
        G.Context (name, meta)

type G with
    static member AsDisplay (code : string) (lines : Lines) =
        """[<StructuredFormatDisplay("{AsDisplay}")>]"""
        :: lines @ [
            "    member this.AsDisplay ="
            "        " + code
        ]
    static member Default (code : string) (lines : Lines) =
        lines @ [
            "    static member Default () ="
            "        " + code
        ]
    static member ToString (code : string) (lines : Lines) =
        lines @ [
            "    override this.ToString () ="
            "        " + code
        ]
[<AutoOpen>]
module Dap.Context.Generator.Helper

open Microsoft.FSharp.Quotations

open Dap.Prelude
open Dap.Context
open Dap.Context.Meta
open Dap.Context.Meta.Util
open Dap.Context.Generator.Util

let private getHeader (keepCommentMode : bool) (param : IParam) (meta : obj) =
    []
    |> List.append (
        [
            sprintf "(*"
            sprintf " * Generated: <%s>" param.Category
        ] @ (if param.Desc <> "" then [sprintf " *     %s" param.Desc] else [])
    ) |> List.extend (if keepCommentMode then [] else [" *)"])

let private getFallbackLines (param : IParam) (meta : obj) =
    [
        sprintf " * Generation failed: generator not found"
        sprintf " *)"
    ]
    |> List.append ^<| getHeader true param meta

let generate<'param when 'param :> IParam> (meta : obj) (getGenerator : obj -> IGenerator<'param> option) (param : 'param) =
    getGenerator meta
    |> Option.map(fun generator ->
        generator.Generate param
        |> List.append ^<| getHeader false param meta
    )|> Option.defaultWith (fun () ->
        getFallbackLines param meta
    )

let getInterfaceGenerator (meta : obj) =
    match meta with
    | :? ComboMeta as combo ->
        new Combo.InterfaceGenerator (combo)
        :> IGenerator<InterfaceParam> |> Some
    | _ -> None

let getRecordGenerator (meta : obj) =
    match meta with
    | :? ComboMeta as combo ->
        new Combo.RecordGenerator (combo)
        :> IGenerator<RecordParam> |> Some
    | _ -> None

let getUnionGenerator (meta : obj) =
    try
        let cases = meta :?> CaseMeta list
        new Union.UnionGenerator (cases)
        :> IGenerator<UnionParam> |> Some
    with e ->
        None

let getClassGenerator (meta : obj) =
    match meta with
    | :? ComboMeta as combo ->
        new Combo.ClassGenerator (combo)
        :> IGenerator<ClassParam> |> Some
    | _ -> None

let getBuilderGenerator (meta : obj) =
    match meta with
    | :? ComboMeta as combo ->
        new Combo.BuilderGenerator (combo)
        :> IGenerator<BuilderParam> |> Some
    | _ -> None

type internal ModuleGenerator (sections : Lines list) =
    member __.Generate (param : ModuleParam) : Lines =
        sections
        |> List.map (fun section ->
            "" :: section
        )|> List.concat
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
    static member ComboInterface (name, meta, parents) =
        InterfaceParam.Create InterfaceType.ComboInterface name parents
        |> generate meta getInterfaceGenerator
    static member ComboInterface (name, meta) =
        G.ComboInterface (name, meta, [])
    static member ComboInterface (expr : Expr<'obj>, parents) =
        let (name, meta) = unquotePropertyGetExpr expr
        G.ComboInterface (name, meta, parents)
    static member ComboInterface (expr : Expr<'obj>) =
        G.ComboInterface (expr, [])
    static member ValueInterface (name, meta, parents) =
        InterfaceParam.Create InterfaceType.ValueInterface name parents
        |> generate meta getInterfaceGenerator
    static member ValueInterface (name, meta) =
        G.ValueInterface (name, meta, [])
    static member ValueInterface (expr : Expr<'obj>, parents) =
        let (name, meta) = unquotePropertyGetExpr expr
        G.ValueInterface (name, meta, parents)
    static member ValueInterface (expr : Expr<'obj>) =
        G.ValueInterface (expr, [])
    static member Record (name, isJson, isLoose, meta) =
        RecordParam.Create name isJson isLoose
        |> generate meta getRecordGenerator
    static member Record (name, meta) =
        G.Record (name, false, false,  meta)
    static member Record (expr) =
        let (name, meta) = unquotePropertyGetExpr expr
        G.Record (name, meta)
    static member Record (expr) =
        G.Record (expr, [])
    static member JsonRecord (name, meta) =
        G.Record (name, true, false, meta)
    static member JsonRecord (expr) =
        let (name, meta) = unquotePropertyGetExpr expr
        G.JsonRecord (name, meta)
    static member JsonRecord (expr) =
        G.JsonRecord (expr, [])
    static member LooseJsonRecord (name, meta) =
        G.Record (name, true, true, meta)
    static member LooseJsonRecord (expr) =
        let (name, meta) = unquotePropertyGetExpr expr
        G.LooseJsonRecord (name, meta)
    static member LooseJsonRecord (expr) =
        G.LooseJsonRecord (expr, [])
    static member Union (name, isJson, meta) =
        UnionParam.Create name isJson
        |> generate meta getUnionGenerator
    static member Union (name, meta) =
        G.Union (name, false, meta)
    static member Union (expr) =
        let (name, meta) = unquotePropertyGetExpr expr
        G.Union (name, meta)
    static member JsonUnion (name, meta) =
        G.Union (name, true, meta)
    static member JsonUnion (expr) =
        let (name, meta) = unquotePropertyGetExpr expr
        G.JsonUnion (name, meta)
    static member Class (name, isAbstract, isFinal, meta) =
        ClassParam.Create name isAbstract isFinal
        |> generate meta getClassGenerator
    static member AbstractClass (name, meta) =
        G.Class (name, true, false, meta)
    static member AbstractClass (expr) =
        let (name, meta) = unquotePropertyGetExpr expr
        G.AbstractClass (name, meta)
    static member FinalClass (name, meta) =
        G.Class (name, false, true, meta)
    static member FinalClass (expr) =
        let (name, meta) = unquotePropertyGetExpr expr
        G.FinalClass (name, meta)
    static member BaseClass (name, meta) =
        G.Class (name, false, false, meta)
    static member BaseClass (expr) =
        let (name, meta) = unquotePropertyGetExpr expr
        G.BaseClass (name, meta)
    static member ComboBuilder (name, kind, key, meta) =
        BuilderParam.Create ComboBuilder name kind key
        |> generate meta getBuilderGenerator
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
    static member ValueBuilder (name, kind, key, meta) =
        BuilderParam.Create ValueBuilder name kind key
        |> generate meta getBuilderGenerator
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
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
        |> List.append [
            if param.AutoOpen then
                yield sprintf "[<AutoOpen>]"
            if param.RequireQualifiedAccess then
                yield sprintf "[<RequireQualifiedAccess>]"
            yield sprintf "module %s" param.Name
        ]

let generateModule sections param =
    ModuleGenerator(sections) .Generate param

let writeCodeFile (path : string) (lines : Lines) =
    let content = lines |> String.concat "\n"
    System.IO.File.WriteAllText (path, content)
    sprintf "Code File Generated: %s -> %d Lines" path lines.Length

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
                "open Dap.Context"
            ] :: sections
        ModuleParam.Create name autoOpen requireQualifiedAccess
        |> generateModule sections
    static member Module (name, sections : Lines list) =
        G.Module (name, true, false, sections)
    static member Module (name, section : Lines, sections : Lines list) =
        G.Module (name, section :: sections)
    static member QualifiedModule (name, autoOpen : bool, sections : Lines list) =
        G.Module (name, autoOpen, true, sections)
    static member QualifiedModule (name, sections : Lines list) =
        G.QualifiedModule (name, true, sections)
    static member QualifiedModule (name, autoOpen : bool, section : Lines, sections : Lines list) =
        G.QualifiedModule (name, autoOpen, section :: sections)
    static member QualifiedModule (name, section : Lines, sections : Lines list) =
        G.QualifiedModule (name, section :: sections)
    static member BuilderModule (name, sections) =
        let sections =
            [
                "open Dap.Context"
                "open Dap.Context.Builder"
            ] :: sections
        ModuleParam.Create name false false
        |> generateModule sections
    static member BuilderModule (name, section : Lines, sections : Lines list) =
        G.BuilderModule (name, section :: sections)
    static member Interface (face : Interface) =
        face.Param
        |> generate face.Meta getInterfaceGenerator
    static member Record (name, isJson, isLoose, interfaces, meta) =
        RecordParam.Create name isJson isLoose interfaces
        |> generate meta getRecordGenerator
    static member Record (name, interfaces, meta) =
        G.Record (name, false, false, interfaces, meta)
    static member Record (expr, interfaces) =
        let (name, meta) = unquoteTemplate expr
        G.Record (name, interfaces, meta)
    static member Record (expr) =
        G.Record (expr, [])
    static member JsonRecord (name, interfaces, meta) =
        G.Record (name, true, false, interfaces, meta)
    static member JsonRecord (expr, interfaces) =
        let (name, meta) = unquoteTemplate expr
        G.JsonRecord (name, interfaces, meta)
    static member JsonRecord (expr) =
        G.JsonRecord (expr, [])
    static member LooseJsonRecord (name, interfaces, meta) =
        G.Record (name, true, true, interfaces, meta)
    static member LooseJsonRecord (expr, interfaces) =
        let (name, meta) = unquoteTemplate expr
        G.LooseJsonRecord (name, interfaces, meta)
    static member LooseJsonRecord (expr) =
        G.JsonRecord (expr, [])
    static member Union (name, isJson, meta) =
        UnionParam.Create name isJson
        |> generate meta getUnionGenerator
    static member Union (name, meta) =
        G.Union (name, false, meta)
    static member Union (expr) =
        let (name, meta) = unquoteTemplate expr
        G.Union (name, meta)
    static member JsonUnion (name, meta) =
        G.Union (name, true, meta)
    static member JsonUnion (expr) =
        let (name, meta) = unquoteTemplate expr
        G.JsonUnion (name, meta)
    static member Class (name, kind, isAbstract, isFinal, interfaces, meta) =
        ClassParam.Create name kind isAbstract isFinal interfaces
        |> generate meta getClassGenerator
    static member AbstractClass (name, kind, interfaces, meta) =
        G.Class (name, kind, true, false, interfaces, meta)
    static member AbstractClass (name, interfaces, meta) =
        G.AbstractClass (name, name, interfaces, meta)
    static member AbstractClass (expr, interfaces) =
        let (name, meta) = unquoteTemplate expr
        G.AbstractClass (name, interfaces, meta)
    static member FinalClass (name, kind, interfaces, meta) =
        G.Class (name, kind, false, true, interfaces, meta)
    static member FinalClass (name, interfaces, meta) =
        G.FinalClass (name, name, interfaces, meta)
    static member FinalClass (expr, interfaces) =
        let (name, meta) = unquoteTemplate expr
        G.FinalClass (name, interfaces, meta)
    static member BaseClass (name, kind, interfaces, meta) =
        G.Class (name, kind, false, false, interfaces, meta)
    static member BaseClass (name, interfaces, meta) =
        G.BaseClass (name, name, interfaces, meta)
    static member BaseClass (expr, interfaces) =
        let (name, meta) = unquoteTemplate expr
        G.BaseClass (name, interfaces, meta)
    static member Builder (key, name, kind, meta) =
        BuilderParam.Create key name kind
        |> generate meta getBuilderGenerator
    static member Builder (key : string, kind, meta) =
        let name = sprintf "%sBuilder" key.AsCodeMemberName
        G.Builder (key, name, kind, meta)
    static member Builder (key : string, expr) =
        let (kind, meta) = unquoteTemplate expr
        G.Builder (key, kind, meta)
    static member Builder (expr) =
        let (kind, meta) = unquoteTemplate expr
        let key = kind.AsCodeVariableName
        G.Builder (key, kind, meta)
    static member AsDisplay (code : string) (lines : Lines) =
        """[<StructuredFormatDisplay("{AsDisplay}")>]"""
        :: lines @ [
            "    member this.AsDisplay ="
            "        " + code
        ]
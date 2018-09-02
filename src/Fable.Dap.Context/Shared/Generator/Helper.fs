[<AutoOpen>]
module Dap.Context.Generator.Helper

open Dap.Prelude
open Dap.Context
open Dap.Context.Generator.Util

let private getHeader (keepCommentMode : bool) (param : IParam) (template : IObj) =
    match template with
    | :? IProperty as prop ->
        E.encodeJson 4 prop
        |> fun j -> j.Split ('\n')
        |> Array.toList
        |> List.map ^<| sprintf "    %s"
    | _ ->
        [
            sprintf " * Unsupported template: <%s>" ((template.GetType ()).FullName)
        ]
    |> List.append (
        [
            sprintf "(*"
            sprintf " * Generated: %s<%s>" param.Category param.Name
        ] @ (if param.Desc <> "" then [sprintf " *     %s" param.Desc] else [])
    ) |> List.extend (if keepCommentMode then [] else [" *)"])

let private getFallbackLines (param : IParam) (template : IObj) =
    [
        sprintf " * Generation failed: generator not found"
        sprintf " *)"
    ]
    |> List.append ^<| getHeader true param template

let generate<'param when 'param :> IParam> (template : IObj)(getGenerator : IObj -> IGenerator<'param> option) (param : 'param) =
    getGenerator template
    |> Option.map(fun generator ->
        generator.Generate param
        |> List.append ^<| getHeader false param template
    )|> Option.defaultWith (fun () ->
        getFallbackLines param template
    )

let getInterfaceGenerator (template : IObj) =
    match template with
    | :? IComboProperty as combo ->
        new Combo.InterfaceGenerator (combo)
        :> IGenerator<InterfaceParam> |> Some
    | _ -> None

let getRecordGenerator (template : IObj) =
    match template with
    | :? IComboProperty as combo ->
        new Combo.RecordGenerator (combo)
        :> IGenerator<RecordParam> |> Some
    | _ -> None

let getClassGenerator (template : IObj) =
    match template with
    | :? IComboProperty as combo ->
        new Combo.ClassGenerator (combo)
        :> IGenerator<ClassParam> |> Some
    | _ -> None

let getBuilderGenerator (template : IObj) =
    match template with
    | :? IComboProperty as combo ->
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
    static member Module (name, autoOpen, sections) =
        let sections =
            [
                "open Dap.Context"
            ] :: sections
        ModuleParam.Create name autoOpen
        |> generateModule sections
    static member Module (name, sections : Lines list) =
        G.Module (name, true, sections)
    static member Module (name, section : Lines, sections : Lines list) =
        G.Module (name, section :: sections)
    static member BuilderModule (name, sections) =
        let sections =
            [
                "open Dap.Context"
                "open Dap.Context.Builder"
            ] :: sections
        ModuleParam.Create name false
        |> generateModule sections
    static member BuilderModule (name, section : Lines, sections : Lines list) =
        G.BuilderModule (name, section :: sections)
    static member Interface (face : Interface) =
        face.Param
        |> generate face.Template getInterfaceGenerator
    static member Record (name, isJson, isLoose, interfaces, template) =
        RecordParam.Create name isJson isLoose interfaces
        |> generate template getRecordGenerator
    static member Record (name, interfaces, template) =
        G.Record (name, false, false, interfaces, template)
    static member JsonRecord (name, interfaces, template) =
        G.Record (name, true, false, interfaces, template)
    static member LooseJsonRecord (name, interfaces, template) =
        G.Record (name, true, true, interfaces, template)
    static member Class (name, kind, isAbstract, isFinal, interfaces, template) =
        ClassParam.Create name kind isAbstract isFinal interfaces
        |> generate template getClassGenerator
    static member AbstractClass (name, kind, interfaces, template) =
        G.Class (name, kind, true, false, interfaces, template)
    static member AbstractClass (name, interfaces, template) =
        G.AbstractClass (name, name, interfaces, template)
    static member FinalClass (name, kind, interfaces, template) =
        G.Class (name, kind, false, true, interfaces, template)
    static member FinalClass (name, interfaces, template) =
        G.FinalClass (name, name, interfaces, template)
    static member BaseClass (name, kind, interfaces, template) =
        G.Class (name, kind, false, false, interfaces, template)
    static member BaseClass (name, interfaces, template) =
        G.BaseClass (name, name, interfaces, template)
    static member Builder (key, name, kind, template) =
        BuilderParam.Create key name kind
        |> generate template getBuilderGenerator
    static member Builder (key : string, kind, template) =
        let name = sprintf "%sBuilder" key.AsCamelCase
        G.Builder(key, name, kind, template)
    static member Builder (kind : string, template) =
        let key = kind.AsLowerCamelCase
        let name = sprintf "%sBuilder" kind
        G.Builder(key, name, kind, template)

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
            sprintf " * Generated: %s<%s>" param.Kind param.Name
        ] @ (if param.Desc <> "" then [sprintf " *     %s" param.Desc] else [])
    ) |> List.extend (if keepCommentMode then [] else [" *)"])

let private getFallbackLines (param : IParam) (template : IObj) =
    [
        sprintf " * Generation failed: generator not found"
        sprintf " *)"
    ]
    |> List.append ^<| getHeader true param template

let getRecordGenerator (template : IObj) : IRecordGenerator option =
    match template with
    | :? IComboProperty as combo ->
        new Combo.RecordGenerator (combo)
        :> IRecordGenerator |> Some
    | _ -> None

let generateRecord param template =
    getRecordGenerator template
    |> Option.map(fun generator ->
        generator.Generate param
        |> List.append ^<| getHeader false param template
    )|> Option.defaultWith (fun () ->
        getFallbackLines param template
    )

let getClassGenerator (template : IObj) : IClassGenerator option =
    match template with
    | :? IComboProperty as combo ->
        new Combo.ClassGenerator (combo)
        :> IClassGenerator |> Some
    | _ -> None

let generateClass param template =
    getClassGenerator template
    |> Option.map(fun generator ->
        generator.Generate param
        |> List.append ^<| getHeader false param template
    )|> Option.defaultWith (fun () ->
        getFallbackLines param template
    )

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

let generateModule param sections =
    let sections =
        [
            "open Dap.Context"
        ] :: sections
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
        let param = ModuleParam.Create name autoOpen
        generateModule param sections
    static member Module (name, sections) =
        G.Module (name, true, sections)
    static member Record (name, isJson, isLoose, template) =
        let param = RecordParam.Create name isJson isLoose
        generateRecord param template
    static member Record (name, template) =
        G.Record (name, false, false, template)
    static member JsonRecord (name, template) =
        G.Record (name, true, false, template)
    static member LooseJsonRecord (name, template) =
        G.Record (name, true, true, template)
    static member Class (name, kind, isAbstract, isFinal, template) =
        let param = ClassParam.Create name kind isAbstract isFinal
        generateClass param template
    static member AbstractClass (name, kind, template) =
        G.Class (name, kind, true, false, template)
    static member AbstractClass (name, template) =
        G.AbstractClass (name, name, template)
    static member FinalClass (name, kind, template) =
        G.Class (name, kind, false, true, template)
    static member FinalClass (name, template) =
        G.FinalClass (name, name, template)
    static member BaseClass (name, kind, template) =
        G.Class (name, kind, false, false, template)
    static member BaseClass (name, template) =
        G.BaseClass (name, name, template)

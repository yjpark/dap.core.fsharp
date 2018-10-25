module Dap.Context.Generator.Util

open System.Text
open System.Globalization

open Dap.Prelude
open Dap.Context
open Dap.Context.Meta.Util

let private textInfo = (new CultureInfo("en-US", false)) .TextInfo

let private split (key : string) =
    key.Split('_')
    |> Array.map (fun segment ->
        let mutable word = ""
        let mutable words = []
        for ch in segment do
            if ch >= 'A' && ch <= 'Z' then
                if word <> "" then
                    words <- word :: words
                word <- ch.ToString ()
            else
                word <- word + ch.ToString ()
        if word <> "" then
            words <- word :: words
        words |> List.rev
    )|> List.concat

let private toMemberName (key : string) =
    split key
    |> List.map textInfo.ToTitleCase
    |> String.concat ""
    |> fun s -> s.Replace ("'", "")

let private toVariableName (kind : string) =
    split kind
    |> List.map textInfo.ToTitleCase
    |> String.concat ""
    |> fun v ->
        if v.Length = 0 then
            v
        else
            let head = v.Substring (0, 1)
            let head = head.ToLower ()
            sprintf "%s%s" head <| v.Substring (1, v.Length - 1)
    |> escapeKeyword

let private toJsonKey (kind : string) =
    split kind
    |> List.map (fun word -> word.ToLower ())
    |> String.concat "_"
    |> fun s -> s.Replace ("'", "")

let private toClassName (name : string) =
    if name.Length <= 1 then
        name
    else
        let className =
            if name.StartsWith("I") then
                name.Substring (1, name.Length - 1)
            else
                name
        if System.Char.IsUpper (className.[0]) then
            className
        else
            name

let private toInterfaceName (name : string) =
    if name.Length <= 1 then
        "I" + name
    elif name.StartsWith("I") then
        let className = name.Substring (1, name.Length - 1)
        if System.Char.IsUpper (className.[0]) then
            name
        else
            "I" + name
    else
        "I" + name

type System.String with
    member this.AsCodeMemberName = toMemberName this
    member this.AsCodeVariableName = toVariableName this
    member this.AsCodeJsonKey = toJsonKey this
    member this.AsCodeClassName = toClassName this
    member this.AsCodeInterfaceName = toInterfaceName this

let removeDuplicatedLines (filter : string -> bool) (lines : string list) =
    lines
    |> List.fold (fun state line ->
        if filter line then
            state
            |> List.exists (fun l -> l = line)
            |> function
                | true -> state
                | false -> state @ [ line ]
        else
            state @ [ line ]
    ) []

let isModuleAliasLine (line : string) =
    line.StartsWith "module " && (line.IndexOf (" = ") >= 0)

let isModuleOpenLine (line : string) =
    line.StartsWith "open "

let addEmptyLine (lines : string list) =
    match lines with
    | [] -> []
    | _ -> lines @ [""]

let removeMultipleEmptyLine (lines : string list) =
    let mutable lastIsEmpty = true
    lines
    |> List.fold (fun state line ->
        if (line = "") then
            let keep = not lastIsEmpty
            lastIsEmpty <- true
            keep
        else
            lastIsEmpty <- false
            true
        |> function
            | true -> state @ [line]
            | false -> state
    ) []

let standardizeModuleLines (lines : string list) =
    let (opens, lines) =
        lines
        |> List.partition isModuleOpenLine
    let (aliases, lines) =
        lines
        |> List.partition isModuleAliasLine
    let cleanup = fun section ->
        section
        |> removeDuplicatedLines (fun _ -> true)
        |> addEmptyLine
        |> removeMultipleEmptyLine
    cleanup opens @ cleanup aliases @ removeMultipleEmptyLine lines

let indentLines (lines : string list) =
    lines
    |> List.map (fun line -> "    " + line)

let featureToSwitch (feature : string) =
    (feature.ToUpper ()) .Replace (".", "_")
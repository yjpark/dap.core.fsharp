module Dap.Context.Generator.Util

open System.Text
open System.Globalization

open Dap.Prelude
open Dap.Context
open Dap.Context.Meta.Util

let private textInfo = (new CultureInfo("en-US", false)) .TextInfo

let private toKind (key : string) =
    key.Split('_')
    |> Array.map textInfo.ToTitleCase
    |> String.concat ""
    |> fun s -> s.Replace ("'", "")

let private toKey (kind : string) =
    kind.Split('_')
    |> Array.map textInfo.ToTitleCase
    |> String.concat ""
    |> fun v ->
        if v.Length = 0 then
            v
        else
            let head = v.Substring (0, 1)
            let head = head.ToLower ()
            sprintf "%s%s" head <| v.Substring (1, v.Length - 1)
    |> escapeKeyword

type System.String with
    member this.AsCodeMemberName = toKind this
    member this.AsCodeVariableName = toKey this
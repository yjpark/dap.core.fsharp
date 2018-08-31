module Dap.Context.Generator.Util

open System.Text
open System.Globalization

open Dap.Prelude
open Dap.Context

let sf = sprintf

let private textInfo = (new CultureInfo("en-US", false)) .TextInfo

let toCamelCase (key : string) =
    key.Split('_')
    |> Array.map textInfo.ToTitleCase
    |> String.concat ""
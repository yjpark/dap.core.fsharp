module Dap.Context.Generator.Util

open System.Text
open System.Globalization

open Dap.Prelude
open Dap.Context

let private textInfo = (new CultureInfo("en-US", false)) .TextInfo

let private TIMESTAMP_FORMAT = "yyyy-MM-ddTHH:mm:ss";

let getTimestamp () =
    System.DateTime.UtcNow.ToString TIMESTAMP_FORMAT

let toCamelCase (key : string) =
    key.Split('_')
    |> Array.map textInfo.ToTitleCase
    |> String.concat ""

type System.String with
    member this.AsCamelCase = toCamelCase this
[<AutoOpen>]
module Dap.Platform.Native

open Fable.Core
open Fable.Core.JsInterop

open Dap.Prelude
open Dap.Context

[<Import("calcSha256Sum", "./Native/forge_helper.js")>]
let calcSha256Sum' : string -> string = jsNative

let calcSha256Sum (content : string) =
    calcSha256Sum' content
    |> Base64.encode'
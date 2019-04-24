[<AutoOpen>]
module Dap.Platform.Native

open Fable.Core
open Fable.Core.JsInterop

open Dap.Prelude
open Dap.Context

[<Import("calcSum", "Native/sha256.js")>]
let calcSha256Sum' : string -> Bytes = jsNative

let calcSha256Sum (content : string) =
    calcSha256Sum' content
    |> Base64.encode
[<AutoOpen>]
[<RequireQualifiedAccess>]
module Dap.Context.Base64

open System

open Dap.Prelude
let encode (bytes : Bytes) : string =
    bytes
    |> Convert.ToBase64String
    |> (fun str ->
        str.Replace("/", "_")
            .Replace("+", "-")
            .Replace("=", "")
    )

let decode (str : string) : Bytes =
    match str.Length % 4 with
    | 3 -> str + "="
    | 2 -> str + "=="
    | 1 -> failWith "Invalid_Base64" str
    | _ -> str
    |> (fun str ->
        str.Replace("_", "/")
            .Replace("-", "+")
    )|> Convert.FromBase64String

let encodeGuid (guid : Guid) =
    guid.ToByteArray()
    |> encode

let decodeGuid (str : string) =
    decode str
    |> Guid

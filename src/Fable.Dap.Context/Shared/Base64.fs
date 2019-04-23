[<AutoOpen>]
[<RequireQualifiedAccess>]
module Dap.Context.Base64

open System

let encode (bytes : Bytes) : string =
    bytes
    |> Convert.ToBase64String
    |> (fun str ->
        str.Replace("/", "_")
            .Replace("+", "-")
            .Replace("=", "")
    )

let decode (str : string) : Bytes =
    match str.Length % 3 with
    | 1 -> str + "=="
    | 2 -> str + "="
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

[<AutoOpen>]
[<RequireQualifiedAccess>]
module Dap.Platform.Sha256

open Dap.Prelude
open Dap.Context

let computeHash (content : Bytes) : Bytes =
    use sha256 = System.Security.Cryptography.SHA256.Create()
    sha256.ComputeHash (content)

let encodeHash (hash : Bytes) : string =
    hash
    |> Array.map (fun b -> b.ToString ("x2"))
    |> String.concat ""

let ofBytes (content : Bytes) =
    computeHash content |> encodeHash

let ofText (content : string) : string =
    if content = "" || content =? null then
        ""
    else
        ofBytes (System.Text.Encoding.UTF8.GetBytes content)

let ofText2 (content : string) (salt : string) : string =
    ofText <| content + salt

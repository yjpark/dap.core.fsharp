[<AutoOpen>]
[<RequireQualifiedAccess>]
module Dap.Context.Des

open System
open System.Text
open System.Security.Cryptography

open Dap.Prelude

//https://www.c-sharpcorner.com/UploadFile/dhananjaycoder/triple-des-encryption-and-decryption-using-user-provided-key/
let private createDes (key : string) =
    let md5 = new MD5CryptoServiceProvider()
    let des = new TripleDESCryptoServiceProvider()
    des.Key <- md5.ComputeHash (Encoding.UTF8.GetBytes (key))
    des.IV <- Array.create<byte> (des.BlockSize / 8) 0uy
    des

let encrypt (key : string) (content : string) =
    let des = createDes key
    let ct = des.CreateEncryptor ()
    let input = Encoding.UTF8.GetBytes (content)
    let output = ct.TransformFinalBlock (input, 0, input.Length)
    Convert.ToBase64String (output)

let decrypt (runner : ILogger) (key : string) (content : string) =
    try
        let des = createDes key
        let ct = des.CreateDecryptor ()
        let input = Convert.FromBase64String (content)
        let output = ct.TransformFinalBlock (input, 0, input.Length)
        Some <| Encoding.UTF8.GetString (output, 0, output.Length)
    with e ->
        logException runner "Des" "Decrypt_Failed" (key, content) e
        None

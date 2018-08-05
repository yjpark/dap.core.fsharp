[<AutoOpen>]
[<RequireQualifiedAccess>]
module Dap.Prelude.String

let capped (limit : int) (str : string) =
    if str.Length <= limit then
        str
    else
        sprintf "[%i] %s ..." str.Length <| str.Substring (0, limit)
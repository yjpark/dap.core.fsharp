[<AutoOpen>]
[<RequireQualifiedAccess>]
module Dap.Prelude.Bool

let iter action = function
    | true -> action ()
    | false -> ()

let iterElse action = function
    | true -> ()
    | false -> action ()

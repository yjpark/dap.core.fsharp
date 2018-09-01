[<AutoOpen>]
[<RequireQualifiedAccess>]
module Dap.Prelude.List

let iterBack (func : 'a -> unit) (list : 'a list) =
    let folder = fun elem () ->
        func elem
        ()
    List.foldBack folder list ()

let extend (list2 : 'a list) (list1 : 'a list) =
    list1 @ list2
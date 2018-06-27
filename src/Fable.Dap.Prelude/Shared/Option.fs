[<AutoOpen>]
[<RequireQualifiedAccess>]
module Dap.Prelude.Option

let bind2 (binder : 'a -> 'b -> Option<'r> ) (a : Option<'a>) (b : Option<'b>) =
    match (a, b) with
    | (Some a, Some b) -> binder a b
    | _ -> None

let bind3 (binder : 'a -> 'b -> 'c -> Option<'r> ) (a : Option<'a>) (b : Option<'b>) (c : Option<'c>) =
    match (a, b, c) with
    | (Some a, Some b, Some c) -> binder a b c
    | _ -> None

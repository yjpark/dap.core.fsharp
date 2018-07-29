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

let toResult (err : 'err) (a : Option<'a>) : Result<'a, 'err> =
    match a with
    | Some a -> Ok a
    | None -> Error err

let toResultWith (toErr : unit -> 'err) (a : Option<'a>) : Result<'a, 'err> =
    match a with
    | Some a -> Ok a
    | None -> Error <| toErr ()

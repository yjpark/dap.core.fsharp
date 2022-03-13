[<AutoOpen>]
[<RequireQualifiedAccess>]
module Dap.Prelude.Option

#if !FABLE_COMPILER
open System.Threading
open System.Threading.Tasks
#endif

type Option<'T> with
    member this.Value = Option.get this

let bind2 (binder : 'a -> 'b -> Option<'r> ) (a : Option<'a>) (b : Option<'b>) =
    match (a, b) with
    | (Some a, Some b) -> binder a b
    | _ -> None

let bind3 (binder : 'a -> 'b -> 'c -> Option<'r> ) (a : Option<'a>) (b : Option<'b>) (c : Option<'c>) =
    match (a, b, c) with
    | (Some a, Some b, Some c) -> binder a b c
    | _ -> None

let map2 (mapper : 'a -> 'b -> 'r ) (a : Option<'a>) (b : Option<'b>) =
    match (a, b) with
    | (Some a, Some b) -> Some <| mapper a b
    | _ -> None

let map3 (mapper : 'a -> 'b -> 'c -> 'r ) (a : Option<'a>) (b : Option<'b>) (c : Option<'c>) =
    match (a, b, c) with
    | (Some a, Some b, Some c) -> Some <| mapper a b c
    | _ -> None

let iter2 (func : 'a -> 'b -> unit ) (a : Option<'a>) (b : Option<'b>) =
    match (a, b) with
    | (Some a, Some b) -> func a b
    | _ -> ()

let iter3 (func : 'a -> 'b -> 'c -> unit ) (a : Option<'a>) (b : Option<'b>) (c : Option<'c>) =
    match (a, b, c) with
    | (Some a, Some b, Some c) -> func a b c
    | _ -> ()

let toResult (err : 'err) (a : Option<'a>) : Result<'a, 'err> =
    match a with
    | Some a -> Ok a
    | None -> Error err

let toResultWith (toErr : unit -> 'err) (a : Option<'a>) : Result<'a, 'err> =
    match a with
    | Some a -> Ok a
    | None -> Error <| toErr ()

let ofTry (mapping : 'a -> 'b) (a : 'a) : Option<'b> =
    try
        mapping a |> Some
    with _e ->
        None

let tryMap (mapping : 'a -> 'b) (a : Option<'a>) : Option<'b> =
    a
    |> Option.bind (fun a -> ofTry mapping a)

#if !FABLE_COMPILER
let mapAsync (mappingAsync : 'T1 -> Task<'T2>) (a : Task<Option<'T1>>) : Task<Option<'T2>> = task {
    match! a with
    | Some a ->
        let! a = mappingAsync a
        return Some a
    | None ->
        return None
}

let bindAsync (binderAsync : 'T1 -> Task<Option<'T2>>) (a : Task<Option<'T1>>) : Task<Option<'T2>> = task {
    match! a with
    | Some a ->
        return! binderAsync a
    | None ->
        return None
}

let getAsync (a : Task<Option<'T>>) : Task<'T> = task {
    let! a = a
    return a |> Option.get
}

let toAsync (a : Option<'T>) : Task<Option<'T>> = task {
    return a
}

let mapTask (mapping : 'T1 -> 'T2) (a : Task<Option<'T1>>) : Task<Option<'T2>> = task {
    match! a with
    | Some a ->
        return Some (mapping a)
    | None ->
        return None
}

let bindTask (binder : 'T1 -> Option<'T2>) (a : Task<Option<'T1>>) : Task<Option<'T2>> = task {
    match! a with
    | Some a ->
        return binder a
    | None ->
        return None
}

#endif

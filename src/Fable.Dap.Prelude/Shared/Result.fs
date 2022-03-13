[<AutoOpen>]
[<RequireQualifiedAccess>]
module Dap.Prelude.Result

#if !FABLE_COMPILER
open System.Threading
open System.Threading.Tasks
#endif

// https://github.com/fsharp/fslang-suggestions/issues/623
let isOk = function
    | Ok _ -> true
    | Error _ -> false

let isError = function
    | Ok _ -> false
    | Error _ -> true

let get = function
    | Ok res -> res
    | Error err ->
        invalidArg "Result_Is_Error" <| err.ToString()

let getError = function
    | Ok res ->
        invalidArg "Result_Is_Ok" <| res.ToString()
    | Error err ->
        err

let getWith (toRes : 'err -> 'a) (a : Result<'a, 'err>) : 'a =
    match a with
    | Ok res -> res
    | Error err -> toRes err

let iter action = function
    | Ok res -> action res
    | Error _ -> ()

let iterError action = function
    | Ok _ -> ()
    | Error err -> action err

type Result<'T, 'TError> with
    member this.IsOk = isOk this
    member this.IsError = isError this
    member this.Value = get this
    member this.ErrorValue = getError this

let ofTry (mapping : 'a -> 'b) (a : 'a) : Result<'b, exn> =
    try
        mapping a |> Ok
    with e ->
        Error e

let tryMap (mapping : 'a -> 'b) (a : Result<'a, exn>) : Result<'b, exn> =
    a
    |> Result.bind (fun a -> ofTry mapping a)

#if !FABLE_COMPILER
let mapErrorAsync (mappingAsync : 'E1 -> Task<'E2>) (result : Task<Result<'T, 'E1>>) : Task<Result<'T, 'E2>> = task {
    match! result with
    | Ok res ->
        return (Ok res)
    | Error err ->
        let! err = mappingAsync err
        return (Error err)
}

let mapAsync (mappingAsync : 'T1 -> Task<'T2>) (result : Task<Result<'T1, 'E>>) : Task<Result<'T2, 'E>> = task {
    match! result with
    | Ok res ->
        let! res = mappingAsync res
        return (Ok res)
    | Error err ->
        return (Error err)
}

let bindAsync (binderAsync : 'T1 -> Task<Result<'T2, 'E>>) (result : Task<Result<'T1, 'E>>) : Task<Result<'T2, 'E>> = task {
    match! result with
    | Ok res ->
        return! binderAsync res
    | Error err ->
        return (Error err)
}

let getAsync (result : Task<Result<'T, 'E>>) : Task<'T> = task {
    let! result = result
    return result |> get
}

let toAsync (result : Result<'T, 'E>) : Task<Result<'T, 'E>> = task {
    return result
}

let mapErrorTask (mapping : 'E1 -> 'E2) (result : Task<Result<'T, 'E1>>) : Task<Result<'T, 'E2>> = task {
    match! result with
    | Ok res ->
        return (Ok res)
    | Error err ->
        let err = mapping err
        return (Error err)
}

let mapTask (mapping : 'T1 -> 'T2) (result : Task<Result<'T1, 'E>>) : Task<Result<'T2, 'E>> = task {
    match! result with
    | Ok res ->
        let res = mapping res
        return (Ok res)
    | Error err ->
        return (Error err)
}

let bindTask (binder : 'T1 -> Result<'T2, 'E>) (result : Task<Result<'T1, 'E>>) : Task<Result<'T2, 'E>> = task {
    match! result with
    | Ok res ->
        return binder res
    | Error err ->
        return (Error err)
}

#endif
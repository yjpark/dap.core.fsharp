[<AutoOpen>]
[<RequireQualifiedAccess>]
module Dap.Prelude.Result

#if !FABLE_COMPILER
open System.Threading
open System.Threading.Tasks
open FSharp.Control.Tasks.V2
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

let ofTry (mapping : 'a -> 'b) (a : 'a) : Result<'b, exn> =
    try
        mapping a |> Ok
    with e ->
        Error e

let tryMap (mapping : 'a -> 'b) (a : Result<'a, exn>) : Result<'b, exn> =
    a
    |> Result.bind (fun a -> ofTry mapping a)

#if !FABLE_COMPILER
let mapError' (mapping : 'E1 -> Async<'E2>) (result : Result<'T, 'E1>) : Result<'T, 'E2> =
    match result with
    | Ok res ->
        Ok res
    | Error err ->
        let err = Async.RunSynchronously <| mapping err
        Error err

let map' (mapping : 'T1 -> Async<'T2>) (result : Result<'T1, 'E>) : Result<'T2, 'E> =
    match result with
    | Ok res ->
        let res = Async.RunSynchronously <| mapping res
        Ok res
    | Error err ->
        Error err

let bind' (binder : 'T1 -> Async<Result<'T2, 'E>>) (result : Result<'T1, 'E>) : Result<'T2, 'E> =
    match result with
    | Ok res ->
        Async.RunSynchronously <| binder res
    | Error err ->
        Error err

let mapErrorAsync (mappingAsync : 'E1 -> Task<'E2>) (result : Task<Result<'T, 'E1>>) : Task<Result<'T, 'E2>> = task {
    let! result = result
    match result with
    | Ok res ->
        return (Ok res)
    | Error err ->
        let! err = mappingAsync err
        return (Error err)
}

let mapAsync (mappingAsync : 'T1 -> Task<'T2>) (result : Task<Result<'T1, 'E>>) : Task<Result<'T2, 'E>> = task {
    let! result = result
    match result with
    | Ok res ->
        let! res = mappingAsync res
        return (Ok res)
    | Error err ->
        return (Error err)
}

let bindAsync (binderAsync : 'T1 -> Task<Result<'T2, 'E>>) (result : Task<Result<'T1, 'E>>) : Task<Result<'T2, 'E>> = task {
    let! result = result
    match result with
    | Ok res ->
        return! binderAsync res
    | Error err ->
        return (Error err)
}

let getAsync (result : Task<Result<'T2, 'E>>) : Task<'T2> = task {
    let! result = result
    return result |> get
}
#endif
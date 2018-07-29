[<AutoOpen>]
[<RequireQualifiedAccess>]
module Dap.Prelude.Result

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
#endif
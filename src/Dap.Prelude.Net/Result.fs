[<AutoOpen>]
[<RequireQualifiedAccess>]
module Dap.Prelude.Result

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
[<AutoOpen>]
[<RequireQualifiedAccess>]
module Dap.Prelude.Task

#if !FABLE_COMPILER
open System.Threading
open System.Threading.Tasks
open FSharp.Control.Tasks.V2
#endif

let mapAsync (mappingAsync : 'T1 -> Task<'T2>) (t : Task<'T1>) : Task<'T2> = task {
    let! v = t
    return! mappingAsync v
}

let map (mapping : 'T1 -> 'T2) (t : Task<'T1>) : Task<'T2> = task {
    let! v = t
    return mapping v
}

let mapList (mapping : 'T1 -> Task<'T2>) (values : 'T1 list) : Task<'T2 list> = task {
    let mutable result = []
    for v in values do
        let! o = mapping v
        result <- o :: result
    return result |> List.rev
}

let chooseList (mapping : 'T1 -> Task<'T2 option>) (values : 'T1 list) : Task<'T2 list> = task {
    let mutable result = []
    for v in values do
        let! o = mapping v
        if o.IsSome then
            result <- o.Value :: result
    return result |> List.rev
}

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

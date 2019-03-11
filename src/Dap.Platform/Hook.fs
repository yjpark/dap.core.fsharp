[<AutoOpen>]
[<RequireQualifiedAccess>]
module Dap.Platform.Hook

open System
open System.Reflection

open Dap.Prelude
open Dap.Context
open Dap.Context.Unsafe

let createOne<'hook when 'hook :> IHook> (logging : ILogging) (type' : Type) : 'hook option =
    try
        Activator.CreateInstance (type', [| (logging :> obj) |])
        :?> 'hook
        |> Some
    with e ->
        logException logging "Hook.createOne" "Exception_Raised" typeof<'hook> e
        None

let createAll<'hook when 'hook :> IHook> (logging : ILogging) : 'hook list =
    let kind = Bootstrap.getKind typeof<'hook>
    getHooks logging
    |> Map.tryFind kind
    |> Option.map (fun types ->
        types
        |> List.choose (createOne<'hook> logging)
    )|> Option.defaultValue []

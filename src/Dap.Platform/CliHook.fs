[<AutoOpen>]
[<RequireQualifiedAccess>]
module Dap.Platform.CliHook

open System
open System.Reflection

open Dap.Prelude
open Dap.Platform.Cli

let createOne<'hook when 'hook :> ICliHook> (logging : ILogging) (type' : Type) : 'hook option =
    try
        Activator.CreateInstance (type', [| |])
        :?> 'hook
        |> Some
    with e ->
        logException logging "CliHook.createOne" "Exception_Raised" typeof<'hook> e
        None

let createAll<'hook when 'hook :> ICliHook> (logging : ILogging) : 'hook list =
    let kind = Bootstrap.getKind typeof<'hook>
    getCliHooks logging
    |> Map.tryFind kind
    |> Option.map (fun types ->
        types
        |> List.choose (createOne<'hook> logging)
    )|> Option.defaultValue []

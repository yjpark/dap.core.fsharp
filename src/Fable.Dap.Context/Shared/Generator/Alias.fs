[<AutoOpen>]
module Dap.Context.Generator.Alias

open Dap.Prelude
open Dap.Context

let getGenerator (template : IObj) : IGenerator option =
    match template with
    | :? IComboProperty as combo ->
        new Combo.Generator (combo)
        :> IGenerator |> Some
    | _ -> None

let generateClass kind template =
    getGenerator template
    |> Option.map(fun generator ->
        generator.GenerateClass kind
    )|> Option.defaultValue []
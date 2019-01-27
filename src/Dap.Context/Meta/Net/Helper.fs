[<AutoOpen>]
module Dap.Context.Meta.Net.Helper

open Microsoft.FSharp.Quotations

open Dap.Prelude
open Dap.Context
open Dap.Context.Meta.Util

type M with
    static member bytes (format : BytesFormat, key, ?value : string, ?validator : string) =
        let value = value |> Option.defaultValue "[| |]"
        match format with
        | BytesFormat.Custom (_encoder, _decoder) ->
            failWith "Invalid_BytesFormat" "Custom"
        | _ ->
            sprintf "BytesFormat.%s" <| Union.getKind format
        |> fun t -> FieldMeta.CreateCustom t key value validator
        |> fun m -> m.ToAlias "Bytes"
    static member bytes ((encoder, decoder) : string * string, key, ?value : string, ?validator : string) =
        let value = value |> Option.defaultValue "[| |]"
        sprintf "(DurationFormat.Custom (%A, %A))" encoder decoder
        |> fun t -> FieldMeta.CreateCustom t key value validator
        |> fun m -> m.ToAlias "Bytes"

[<AutoOpen>]
module Dap.Platform.Meta.Net.Extensions

open Dap.Prelude
open Dap.Context
open Dap.Context.Meta
open Dap.Platform
open Dap.Platform.Meta

type M with
    //TODO: this is not working yet
    static member instant (format : InstantFormat, key, ?value : string, ?validator : string) =
        let value = value |> Option.defaultValue "(getNow' ())"
        match format with
        | InstantFormat.Custom pattern ->
            sprintf "(InstantFormat.Custom %s)" pattern
        | _ ->
            sprintf "InstantFormat.%s" <| Union.getKind format
        |> fun t -> FieldMeta.CreateCustom t key value validator
        |> fun m -> m.ToAlias "Instant"
    //TODO: this is not working yet
    static member instant (key, v : Instant, ?validator : string) =
        let v = InstantFormat.General.Format v
        M.instant (InstantFormat.General, key, value = v, ?validator = validator)

type M with
    //TODO: this is not working yet
    static member duration (key, value : Duration, ?validator : string) =
        M.duration (key, "", ?validator = validator)
        |> fun m -> m.WithValue (E.duration value)
    //TODO: this is not working yet
    static member duration (format : DurationFormat, key, ?value : string, ?validator : string) =
        let value = value |> Option.defaultValue "noDuration"
        match format with
        | DurationFormat.Custom pattern ->
            sprintf "(DurationFormat.Custom %s)" pattern
        | _ ->
            sprintf "DurationFormat.%s" <| Union.getKind format
        |> fun t -> FieldMeta.CreateCustom t key value validator
        |> fun m -> m.ToAlias "Duration"
    //TODO: this is not working yet
    static member duration (format : DurationFormat, key, value : Duration, ?validator : string) =
        M.duration (format, key, "", ?validator = validator)
        |> fun m -> m.WithValue (format.JsonEncoder value)

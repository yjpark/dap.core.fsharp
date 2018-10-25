[<AutoOpen>]
module Dap.Platform.MG

open Dap.Prelude
open Dap.Context
open Dap.Context.Meta

type M with
    static member kind (key, ?value : Kind, ?validator : string) =
        let value = value |> Option.defaultValue "NoKind"
        M.basic ("string", key, value, ?validator=validator)
        |> fun m -> m.ToAlias "Kind"
    static member key (key, ?value : Key, ?validator : string) =
        let value = value |> Option.defaultValue "NoKey"
        M.basic ("string", key, value, ?validator=validator)
        |> fun m -> m.ToAlias "Key"
    static member ident (key, ?value : string, ?validator : string) =
        let value = value |> Option.defaultValue "noIdent"
        M.basic ("ident", key, value, ?validator=validator)
        |> fun m -> m.ToAlias "Ident"

type M with
    static member dateTime (key, ?value : string, ?validator : string) =
        let value = value |> Option.defaultValue "System.DateTime.UtcNow"
        M.basic ("dateTime", key, value, ?validator=validator)
        |> fun m -> m.ToAlias "System.DateTime"
    static member instant (key, ?value : string, ?validator : string) =
        let value = value |> Option.defaultValue "(getNow' ())"
        M.basic ("instant", key, value, ?validator=validator)
        |> fun m -> m.ToAlias "Instant"
#if !FABLE_COMPILER
    static member instant (format : InstantFormat, key, ?value : string, ?validator : string) =
        let value = value |> Option.defaultValue "(getNow' ())"
        match format with
        | InstantFormat.Custom pattern ->
            sprintf "(InstantFormat.Custom %s)" pattern
        | _ ->
            sprintf "InstantFormat.%s" <| Union.getKind format
        |> fun t -> FieldMeta.CreateCustom t key value validator
        |> fun m -> m.ToAlias "Instant"
#endif

type M with
    static member duration (key, ?value : string, ?validator : string) =
        let value = value |> Option.defaultValue "noDuration"
        M.basic ("duration", key, value, ?validator=validator)
        |> fun m -> m.ToAlias "Duration"
#if !FABLE_COMPILER
    static member duration (key, value : Duration, ?validator : string) =
        M.duration (key, "", ?validator = validator)
        |> fun m -> m.WithValue (E.duration value)
    static member duration (format : DurationFormat, key, ?value : string, ?validator : string) =
        let value = value |> Option.defaultValue "noDuration"
        match format with
        | DurationFormat.Custom pattern ->
            sprintf "(DurationFormat.Custom %s)" pattern
        | _ ->
            sprintf "DurationFormat.%s" <| Union.getKind format
        |> fun t -> FieldMeta.CreateCustom t key value validator
        |> fun m -> m.ToAlias "Duration"
    static member duration (format : DurationFormat, key, value : Duration, ?validator : string) =
        M.duration (format, key, "", ?validator = validator)
        |> fun m -> m.WithValue (format.JsonEncoder value)
#endif

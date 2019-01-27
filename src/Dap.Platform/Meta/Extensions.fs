[<AutoOpen>]
module Dap.Platform.Meta.Extensions

open Dap.Prelude
open Dap.Context
open Dap.Context.Meta
open Dap.Platform

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

type M with
    static member duration (key, ?value : string, ?validator : string) =
        let value = value |> Option.defaultValue "noDuration"
        M.basic ("duration", key, value, ?validator=validator)
        |> fun m -> m.ToAlias "Duration"

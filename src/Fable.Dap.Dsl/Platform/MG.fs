[<AutoOpen>]
module Dap.Platform.MG

open Dap.Prelude
open Dap.Context
open Dap.Context.Meta

type M with
    static member ident (key, initValue : string, validator) =
        PropMeta.Create "Ident" "E.ident" "D.ident" "S.ident" VarProperty
            key initValue validator
    static member ident (key, initValue) =
        M.ident (key, initValue, "")
    static member ident (key) =
        M.ident (key, "noIdent")
    static member dateTime (key, initValue : string, validator) =
        PropMeta.Create "System.DateTime" "E.dateTime" "D.dateTime" "S.dateTime" VarProperty
            key initValue validator
    static member dateTime (key, initValue) =
        M.dateTime (key, initValue, "")
    static member dateTime (key) =
        M.dateTime (key, "System.DateTime.UtcNow")
#if !FABLE_COMPILER
    static member instant (key, initValue : string, validator) =
        PropMeta.Create "Instant" "E.instant" "D.instant" "S.instant" VarProperty
            key initValue validator
    static member instant (key, initValue) =
        M.instant (key, initValue, "")
    static member instant (key) =
        M.instant (key, "(getNow' ())")
    static member instant (format : InstantFormat, key, initValue : string, validator) =
        let (encoder, decoder, spec) =
            match format with
            | InstantFormat.Custom pattern ->
                (
                    sprintf "(InstantFormat.Custom %s).JsonEncoder" pattern,
                    sprintf "(InstantFormat.Custom %s).JsonDecoder" pattern,
                    sprintf "(InstantFormat.Custom %s).JsonSpec" pattern
                )
            | _ ->
                let kind = Union.getKind format
                (
                    sprintf "InstantFormat.%s.JsonEncoder" kind,
                    sprintf "InstantFormat.%s.JsonDecoder" kind,
                    sprintf "InstantFormat.%s.JsonSpec" kind
                )
        PropMeta.Create "Instant" encoder decoder spec VarProperty
            key initValue validator
    static member instant (format : InstantFormat, key, initValue) =
        M.instant (format, key, initValue, "")
    static member instant (format : InstantFormat, key) =
        M.instant (format, key, "(getNow' ())")
    static member duration (key, initValue : string, validator) =
        PropMeta.Create "Duration" "E.duration" "D.duration" "S.duration" VarProperty
            key initValue validator
    static member duration (key, initValue : Duration, validator) =
        let initValue = jsonInitValue "Duration" E.duration initValue
        M.duration (key, initValue, validator)
    static member duration (key, initValue : Duration) =
        M.duration (key, initValue, "")
    static member duration (key) =
        M.duration (key, noDuration)
    static member duration (format : DurationFormat, key, initValue : string, validator) =
        let (encoder, decoder, spec) =
            match format with
            | DurationFormat.Custom pattern ->
                (
                    sprintf "(DurationFormat.Custom %s).JsonEncoder" pattern,
                    sprintf "(DurationFormat.Custom %s).JsonDecoder" pattern,
                    sprintf "(DurationFormat.Custom %s).JsonSpec" pattern
                )
            | _ ->
                let kind = Union.getKind format
                (
                    sprintf "DurationFormat.%s.JsonEncoder" kind,
                    sprintf "DurationFormat.%s.JsonDecoder" kind,
                    sprintf "DurationFormat.%s.JsonSpec" kind
                )
        PropMeta.Create "Duration" encoder decoder spec VarProperty
            key initValue validator
    static member duration (format : DurationFormat, key, initValue : Duration, validator) =
        let initValue = jsonInitValue "Duration" format.JsonEncoder initValue
        M.duration (format, key, initValue, validator)
    static member duration (format : DurationFormat, key, initValue : Duration) =
        M.duration (format, key, initValue, "")
    static member duration (format : DurationFormat, key) =
        M.duration (format, key, noDuration)
#endif

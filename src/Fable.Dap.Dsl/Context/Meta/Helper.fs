[<AutoOpen>]
module Dap.Context.Meta.Helper

open Microsoft.FSharp.Quotations

open Dap.Prelude
open Dap.Context
open Dap.Context.Meta.Util

let union = new Union.Builder ()
let fields = new Union.FieldsBuilder ()

let extend (parents : Expr<ComboMeta> list) = new Combo.Builder (parents)
let combo = extend []

[<Literal>]
let response = "response"

let context (properties : Expr<ComboMeta>) = new Context.Builder (properties)

let NoProperties =
    combo {
        nothing ()
    }

type M = MetaBuilderHelper with
    static member coded (t : string, key, value : string, ?validator : string) =
        FieldMeta.CreateCoded t key value validator
    static member basic (t : string, key, value : string, ?validator : string) =
        FieldMeta.CreateBasic t key value validator
    static member custom (t : string, key, value : string, ?validator : string) =
        FieldMeta.CreateCustom t key value validator

let private getInitValue (value : string option) (name : string) (meta : ICustomMeta) =
    value
    |> Option.defaultWith (fun () ->
        meta.GetInitValue name
    )

type M with
    static member custom (expr : Expr<UnionMeta>, key, ?value : string, ?validator : string) =
        let (t, meta) = unquotePropertyGetExpr expr
        let value = getInitValue value t meta
        FieldMeta.CreateCustom t key value validator
    static member custom (expr : Expr<ComboMeta>, key, ?value : string, ?validator : string) =
        let (t, meta) = unquotePropertyGetExpr expr
        let value = getInitValue value t meta
        FieldMeta.CreateCustom t key value validator

type M with
    static member alias (alias : string) (field : FieldMeta) =
        field.ToAlias alias
    static member key (key : string) (field : FieldMeta) =
        {field with Key = key}
    static member comment (comment : string) (field : FieldMeta) =
        field.WithComment <| Some comment

type M with
    static member option (field : FieldMeta) =
        field.ToVariant FieldVariation.Option
    static member list (field : FieldMeta) =
        field.ToVariant FieldVariation.List
    static member dict (field : FieldMeta) =
        field.ToVariant FieldVariation.Dict

type M with
    static member combo (key : string) =
        M.custom ("IComboProperty", key, "")

type M with
    static member unit (key) =
        M.basic ("unit", key, "()")
    static member string (key, ?value : string, ?validator : string) =
        let value = defaultArg value ""
        let value = sprintf "\"%s\"" value
        M.basic ("string", key, value, ?validator=validator)
    static member bool (key, ?value : bool, ?validator : string) =
        let value = defaultArg value false
        let value = if value then "true" else "false"
        M.basic ("bool", key, value, ?validator=validator)
    static member int (key, ?value : int, ?validator : string) =
        let value = defaultArg value 0
        let value = value.ToString ()
        M.basic ("int", key, value, ?validator=validator)
    static member long (key, ?value : int64, ?validator : string) =
        let value = defaultArg value 0L
        let value = sprintf "%sL" (value.ToString ())
        M.basic ("long", key, value, ?validator=validator)
        |> fun m -> m.ToAlias "int64"
    static member float (key, ?value : float, ?validator : string) =
        let value = defaultArg value 0.0
        let value = sprintf "%s" (value.ToString ())
        let value =
            if value.IndexOf (".") >= 0 then
                value
            else
                value + ".0"
        M.basic ("float", key, value, ?validator=validator)
    static member decimal (key, ?value : decimal, ?validator : string) =
        let value = defaultArg value 0M
        let value = sprintf "%sM" (value.ToString ())
        M.basic ("decimal", key, value, ?validator=validator)

type M with
    static member json (key, ?value : Json, ?validator : string) =
        let value = defaultArg value E.nil
        M.basic ("json", key, "", ?validator=validator)
        |> fun m -> m.ToAlias "Json"
        |> fun m -> m.WithValue (value)
    static member guid (key, ?value : string, ?validator : string) =
        let value = defaultArg value "(System.Guid.NewGuid().ToString())"
        M.basic ("string", key, value, ?validator=validator)
        |> fun m -> m.ToAlias "Guid"
    static member luid (key, ?value : string, ?validator : string) =
        let value = defaultArg value "(System.Guid.NewGuid().ToString())"
        M.basic ("string", key, value, ?validator=validator)
        |> fun m -> m.ToAlias "Luid"
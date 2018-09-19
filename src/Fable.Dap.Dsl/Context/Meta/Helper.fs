[<AutoOpen>]
module Dap.Context.Meta.Helper

open Microsoft.FSharp.Quotations

open Dap.Prelude
open Dap.Context
open Dap.Context.Meta.Util

let extend (parents : Expr<ComboMeta> list) = new Combo.Builder (parents)
let combo = extend []

//let context kind = new Context.Builder (kind)

(*
let list<'p when 'p :> IProperty> spawner = new List.Builder<'p> (spawner)
let list0 spawner = new List.Builder<IProperty> (spawner)
*)

let jsonInitValue (type' : string) (encoder : JsonEncoder<'args>) (args : 'args) =
    let json = E.encode 0 <| encoder args
    if json.StartsWith "\"" then
        sprintf "(decodeJsonString %s.JsonDecoder \"\"%s\"\")" type' json
    else
        sprintf "(decodeJsonValue %s.JsonDecoder \"\"\"%s\"\"\")" type' json

let union = Union.CasesBuilder ()
let fields = new Union.FieldsBuilder ()

type M = MetaBuilderHelper with
    static member alias (alias : string) (prop : PropMeta) =
        prop.ToAlias alias
    static member comment (comment : string) (prop : PropMeta) =
        {prop with Comment = Some comment}
    static member hardcoded (name : string, key, initValue) =
        PropMeta.Create name "" "" "" VarProperty
            key initValue ""

type M with
    static member custom (name : string, key, initValue, validator) =
        PropMeta.Create name (name + ".JsonEncoder") (name + ".JsonDecoder") (name + ".JsonSpec") VarProperty
            key initValue validator
    static member custom (name : string, key, initValue) =
        M.custom (name, key, initValue, "")
    static member custom (name : string, key) =
        M.custom (name, key, "")
    static member custom (expr : Expr<'obj>, key, initValue, validator) =
        let (name, _meta) = unquotePropertyGetExpr expr
        M.custom (name, key, initValue, validator)
    static member custom (expr : Expr<'obj>, key, initValue) =
        M.custom (expr, key, initValue, "")
    static member custom (expr : Expr<'obj>, key) =
        M.custom (expr, key, "")

type M with
    static member union (expr : Expr<CaseMeta list>, key, initValue, validator) =
        let (name, cases) = unquotePropertyGetExpr expr
        UnionPropMeta.Create name cases key initValue validator
    static member union (expr, key, initValue) =
        M.union (expr, key, initValue, "")
    static member union (expr, key) =
        M.union (expr, key, "")

type M with
    static member json (key, initValue, validator) =
        PropMeta.Create "Json" "" "D.value" "S.json" VarProperty
            key initValue validator
    static member json (key, initValue) =
        M.json (key, initValue, "")
    static member json (key) =
        M.json (key, "E.nil")

type M with
    static member string (key, initValue : string, validator) =
        let initValue = sprintf "\"%s\"" initValue
        PropMeta.Create "string" "E.string" "D.string" "S.string" VarProperty
            key initValue validator
    static member string (key, initValue) =
        M.string (key, initValue, "")
    static member string (key) =
        M.string (key, "")

type M with
    static member guid (key, initValue : string, validator) =
        PropMeta.Create "Guid" "E.string" "D.string" "S.string" VarProperty
            key initValue validator
    static member guid (key, initValue) =
        M.guid (key, initValue, "")
    static member guid (key) =
        M.guid (key, "(System.Guid.NewGuid().ToString())")

type M with
    static member luid (key, initValue : string, validator) =
        PropMeta.Create "Luid" "E.string" "D.string" "S.string" VarProperty
            key initValue validator
    static member luid (key, initValue) =
        M.luid (key, initValue, "")
    static member luid (key) =
        M.luid (key, "(System.Guid.NewGuid().ToString())")

type M with
    static member bool (key, initValue : string, validator) =
        PropMeta.Create "bool" "E.bool" "D.bool" "S.bool" VarProperty
            key initValue validator
    static member bool (key, initValue : bool, validator) =
        let initValue = if initValue then "true" else "false"
        M.bool (key, initValue, validator)
    static member bool (key, initValue : bool) =
        M.bool (key, initValue, "")
    static member bool (key) =
        M.bool (key, false)

type M with
    static member int (key, initValue : string, validator) =
        PropMeta.Create "int" "E.int" "D.int" "S.int" VarProperty
            key initValue validator
    static member int (key, initValue : int, validator) =
        let initValue = initValue.ToString ()
        M.int (key, initValue, validator)
    static member int (key, initValue : int) =
        M.int (key, initValue, "")
    static member int (key) =
        M.int (key, 0)

#if !FABLE_COMPILER
type M with
    static member long (key, initValue : string, validator) =
        PropMeta.Create "int64" "E.long" "D.long" "S.long" VarProperty
            key initValue validator
    static member long (key, initValue : int64, validator) =
        let initValue = sprintf "%sL" (initValue.ToString ())
        M.long (key, initValue, validator)
    static member long (key, initValue : int64) =
        M.long (key, initValue, "")
    static member long (key) =
        M.long (key, 0L)
#endif

let getJsonInitValue (type' : string) (args : Json) =
    E.encode 0 args
    |> sprintf "(decodeJson %s.JsonDecoder \"\"\"%s\"\"\")" type'

type M with
    static member float (key, initValue : string, validator) =
        PropMeta.Create "float" "E.float" "D.float" "S.float" VarProperty
            key initValue validator
    static member float (key, initValue : float, validator) =
        let initValue = sprintf "%s" (initValue.ToString ())
        let initValue =
            if initValue.IndexOf (".") >= 0 then
                initValue
            else
                initValue + ".0"
        M.float (key, initValue, validator)
    static member float (key, initValue : float) =
        M.float (key, initValue, "")
    static member float (key) =
        M.float (key, 0.0)

type M with
    static member decimal (key, initValue : string, validator) =
        PropMeta.Create "decimal" "E.decimal" "D.decimal" "S.decimal" VarProperty
            key initValue validator
    static member decimal (key, initValue : decimal, validator) =
        let initValue = sprintf "%sM" (initValue.ToString ())
        M.decimal (key, initValue, validator)
    static member decimal (key, initValue : decimal) =
        M.decimal (key, initValue, "")
    static member decimal (key) =
        M.decimal (key, 0M)

[<AutoOpen>]
module Dap.Context.Meta.Helper

open Microsoft.FSharp.Quotations

open Dap.Prelude
open Dap.Context
open Dap.Context.Meta.Util

let combo = new Combo.Builder ([])
let extend parents = new Combo.Builder (parents)

//let context kind = new Context.Builder (kind)

(*
let list<'p when 'p :> IProperty> spawner = new List.Builder<'p> (spawner)
let list0 spawner = new List.Builder<IProperty> (spawner)
*)

let union = Union.CasesBuilder ()
let fields = new Union.FieldsBuilder ()

type M = MetaBuilderHelper with
    static member alias (alias : string) (prop : PropMeta) =
        prop.ToAlias alias
    static member custom (name : string, key, initValue, validator) =
        PropMeta.Create name (name + ".JsonEncoder") (name + ".JsonDecoder") (name + ".JsonSpec") VarProperty
            key initValue validator
    static member custom (name : string, key, initValue) =
        M.custom (name, key, initValue, "")
    static member custom (name : string, key) =
        M.custom (name, key, "")
    static member custom (expr : Expr<ComboMeta>, key, initValue, validator) =
        let (name, _meta) = unquoteTemplate expr
        M.custom (name, key, initValue, validator)
    static member custom (expr : Expr<ComboMeta>, key, initValue) =
        M.custom (expr, key, initValue, "")
    static member custom (expr : Expr<ComboMeta>, key) =
        M.custom (expr, key, "")
    static member union (expr : Expr<CaseMeta list>, key, initValue, validator) =
        let (name, cases) = unquoteTemplate expr
        UnionPropMeta.Create name cases key initValue validator
    static member union (expr, key, initValue) =
        M.union (expr, key, initValue, "")
    static member union (expr, key) =
        M.union (expr, key, "")
    static member json (key, initValue, validator) =
        PropMeta.Create "Json" "" "D.value" "S.json" VarProperty
            key initValue validator
    static member json (key, initValue) =
        M.json (key, initValue, "")
    static member json (key) =
        M.json (key, "E.nil")
    static member string (key, initValue, validator) =
        PropMeta.Create "string" "E.string" "D.string" "S.string" VarProperty
            key initValue validator
    static member string (key, initValue) =
        M.string (key, initValue, "")
    static member string (key) =
        M.string (key, "\"\"")
    static member guid (key, initValue, validator) =
        PropMeta.Create "Guid" "E.string" "D.string" "S.string" VarProperty
            key initValue validator
    static member guid (key, initValue) =
        M.guid (key, initValue, "")
    static member guid (key) =
        M.guid (key, "(System.Guid.NewGuid().ToString())")
    static member luid (key, initValue, validator) =
        PropMeta.Create "Luid" "E.string" "D.string" "S.string" VarProperty
            key initValue validator
    static member luid (key, initValue) =
        M.luid (key, initValue, "")
    static member luid (key) =
        M.luid (key, "(System.Guid.NewGuid().ToString())")
    static member bool (key, initValue, validator) =
        PropMeta.Create "bool" "E.bool" "D.bool" "S.bool" VarProperty
            key initValue validator
    static member bool (key, initValue) =
        M.bool (key, initValue, "")
    static member bool (key) =
        M.bool (key, "false")
    static member int (key, initValue, validator) =
        PropMeta.Create "int" "E.int" "D.int" "S.int" VarProperty
            key initValue validator
    static member int (key, initValue) =
        M.int (key, initValue, "")
    static member int (key) =
        M.int (key, "0")
#if !FABLE_COMPILER
    static member long (key, initValue, validator) =
        PropMeta.Create "int64" "E.long" "D.long" "S.long" VarProperty
            key initValue validator
    static member long (key, initValue) =
        M.long (key, initValue, "")
    static member long (key) =
        M.long (key, "0L")
#endif
    static member decimal (key, initValue, validator) =
        PropMeta.Create "decimal" "E.decimal" "D.decimal" "S.decimal" VarProperty
            key initValue validator
    static member decimal (key, initValue) =
        M.decimal (key, initValue, "")
    static member decimal (key) =
        M.decimal (key, "0M")

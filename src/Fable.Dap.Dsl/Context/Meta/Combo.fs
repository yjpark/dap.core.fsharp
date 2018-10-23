[<RequireQualifiedAccess>]
module Dap.Context.Meta.Combo

open Microsoft.FSharp.Quotations

open Dap.Prelude
open Dap.Context
open Dap.Context.Meta.Util

type Builder (parents : (string * ComboMeta) list) =
    inherit Union.FieldsBuilder<ComboMeta> ()
    new (parents' : Expr<ComboMeta> list) =
        let parents' =
            parents'
            |> List.map unquotePropertyGetExpr
        Builder (parents')
    override __.Zero () = ComboMeta.Create parents []
    override __.AddField field meta =
        meta.AddField field

    [<CustomOperation("nothing")>]
    member __.Nothing (meta : ComboMeta, ()) =
        meta
    [<CustomOperation("prop")>]
    member __.Prop (meta : ComboMeta, prop : FieldMeta) =
        match prop.Type with
        | FieldType.Property _ ->
            meta.AddField prop
        | FieldType.Custom t ->
            let prop = FieldMeta.CreateProperty t prop.Key prop.Value prop.Validator
            meta.AddField prop
        | _ ->
            failWith "Prop" prop.AsString

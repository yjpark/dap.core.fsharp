[<RequireQualifiedAccess>]
module Dap.Context.Meta.Combo

open Microsoft.FSharp.Quotations

open Dap.Prelude
open Dap.Context
open Dap.Context.Meta.Util

type Builder (parents : (string * ComboMeta) list) =
    inherit Union.FieldsBuilder<ComboMeta, IPropMeta> ()
    new (parents' : Expr<ComboMeta> list) =
        let parents' =
            parents'
            |> List.map unquotePropertyGetExpr
        Builder (parents')
    override __.Zero () = ComboMeta.Create parents []
    override __.AddField field fields =
        fields.AddField field

    override __.AddVariant variation field fields =
        field.ToVariant variation
        |> fields.AddField
    [<CustomOperation("nothing")>]
    member __.Nothing (meta : ComboMeta, ()) =
        meta
    [<CustomOperation("prop")>]
    member __.Prop (meta : ComboMeta, prop : PropMeta) =
        match prop.Kind with
        | VarProperty ->
            {prop with Kind = PropertyKind.CustomProperty}
        | _ ->
            prop
        |> meta.AddField

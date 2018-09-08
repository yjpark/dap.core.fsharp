[<RequireQualifiedAccess>]
module Dap.Context.Meta.Combo

open Microsoft.FSharp.Quotations

open Dap.Prelude
open Dap.Context
open Dap.Context.Meta.Util

type Builder (parents' : Expr<ComboMeta> list) =
    inherit Union.FieldsBuilder<ComboMeta, IPropMeta> ()
    let parents =
        parents'
        |> List.map unquoteTemplate
    override __.Zero () = ComboMeta.Create parents []
    override __.AddField field fields =
        fields.AddField field

    override __.AddVariant variation field fields =
        field.ToVariant variation
        :?> IPropMeta
        |> fields.AddField

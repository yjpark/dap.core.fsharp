[<RequireQualifiedAccess>]
module Dap.Context.Meta.Union

open Microsoft.FSharp.Quotations

open Dap.Prelude
open Dap.Context
open Dap.Context.Meta.Util

[<AbstractClass>]
type FieldsBuilder<'fields, 'field> () =
    inherit MetaBuilder<'fields> ()
    abstract AddField : 'field -> 'fields -> 'fields
    abstract AddVariant : FieldVariation -> 'field -> 'fields -> 'fields
    [<CustomOperation("var")>]
    member this.Var (fields : 'fields, field : 'field) =
        fields |> this.AddField field
    [<CustomOperation("option")>]
    member this.Option (fields : 'fields, field : 'field) =
        fields |> this.AddVariant FieldVariation.Option field
    [<CustomOperation("list")>]
    member this.List (fields : 'fields, field : 'field) =
        fields |> this.AddVariant FieldVariation.List field
    [<CustomOperation("dict")>]
    member this.Dict (fields : 'fields, field : 'field) =
        fields |> this.AddVariant FieldVariation.Dict field

type FieldsBuilder () =
    inherit FieldsBuilder<IFieldMeta list, IFieldMeta> ()
    override __.Zero () = []
    override __.AddField field fields =
        fields @ [field]

    override this.AddVariant variation field fields =
        fields
        |> this.AddField ^<| field.ToVariant0 variation

type CasesBuilder () =
    inherit MetaBuilder<CaseMeta list> ()
    override __.Zero () = []
    member __.AddCase (cases : CaseMeta list) (case : CaseMeta) =
        cases @ [case]
    [<CustomOperation("case")>]
    member this.Case (cases : CaseMeta list, kind, fields : IFieldMeta list) =
        CaseMeta.Create kind fields
        |> this.AddCase cases
    [<CustomOperation("kind")>]
    member this.Kind (cases : CaseMeta list, kind) =
        CaseMeta.Create kind []
        |> this.AddCase cases

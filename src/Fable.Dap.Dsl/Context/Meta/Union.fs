[<RequireQualifiedAccess>]
module Dap.Context.Meta.Union

open Microsoft.FSharp.Quotations

open Dap.Prelude
open Dap.Context
open Dap.Context.Meta.Util

[<AbstractClass>]
type FieldsBuilder<'meta> () =
    inherit MetaBuilder<'meta> ()
    abstract AddField : FieldMeta -> 'meta -> 'meta
    [<CustomOperation("var")>]
    member this.Var (meta : 'meta, field : FieldMeta) =
        meta |> this.AddField field
    [<CustomOperation("option")>]
    member this.Option (meta : 'meta, field : FieldMeta) =
        let field = field.ToVariant FieldVariation.Option
        meta |> this.AddField field
    [<CustomOperation("list")>]
    member this.List (meta : 'meta, field : FieldMeta) =
        let field = field.ToVariant FieldVariation.List
        meta |> this.AddField field
    [<CustomOperation("dict")>]
    member this.Dict (meta : 'meta, field : FieldMeta) =
        let field = field.ToVariant FieldVariation.Dict
        meta |> this.AddField field

type FieldsBuilder () =
    inherit FieldsBuilder<FieldMeta list> ()
    override __.Zero () = []
    override __.AddField field fields =
        fields @ [field]

type Builder () =
    inherit MetaBuilder<UnionMeta> ()
    override __.Zero () = UnionMeta.Create []
    [<CustomOperation("case")>]
    member this.Case (meta : UnionMeta, kind, fields : FieldMeta list) =
        CaseMeta.Create kind fields
        |> meta.AddCase
    [<CustomOperation("kind")>]
    member this.Kind (meta : UnionMeta, kind) =
        CaseMeta.Create kind []
        |> meta.AddCase

[<AutoOpen>]
module Dap.Context.Meta.Types

open Dap.Prelude
open Dap.Context
open Dap.Context.Meta.Util

[<AbstractClass>]
type MetaBuilder<'obj> () =
    member this.Yield (_ : 'a) = this.Zero ()
    abstract Zero : unit -> 'obj

type FieldVariation =
    | Nothing
    | Option
    | List
with
    member this.Decorator : string option =
        match this with
        | Nothing -> None
        | Option -> Some "option"
        | List -> Some "list"
    member this.DecorateType type' =
        this.Decorator
        |> Option.map (fun decorator -> sprintf "%s %s" type' decorator)
        |> Option.defaultValue type'
    member this.DecorateEncoder encoder =
        this.Decorator
        |> Option.map (fun decorator -> sprintf "(E.%s %s)" decorator encoder)
        |> Option.defaultValue encoder
    member this.DecorateDecoder decoder =
        this.Decorator
        |> Option.map (fun decorator -> sprintf "(D.%s %s)" decorator decoder)
        |> Option.defaultValue decoder
    member this.DecorateSpec encoder decoder spec =
        this.Decorator
        |> Option.map (fun decorator -> sprintf "(S.%s %s %s)" decorator encoder decoder)
        |> Option.defaultValue spec
    member this.DecorateInitValue initValue =
        match this with
        | Nothing -> initValue
        | Option -> "None"
        | List -> "[]"

type IFieldMeta =
    abstract Type : string with get
    abstract Encoder : string with get
    abstract Decoder : string with get
    abstract Spec : string with get
    abstract Key : string with get
    abstract ToVariant : FieldVariation -> IFieldMeta
    abstract Comment : string option with get

type IPropMeta =
    inherit IFieldMeta
    abstract Kind : PropertyKind with get
    abstract InitValue : string with get
    abstract Validator : string with get

type PropMeta = {
    Type : string
    Encoder : string
    Decoder : string
    Spec : string
    Kind : PropertyKind
    Key : string
    InitValue : string
    Validator : string
    Variation : FieldVariation
    Comment : string option
} with
    static member Create type' encoder decoder spec kind key initValue validator =
        {
            Type = type'
            Encoder = encoder
            Decoder = decoder
            Spec = spec
            Kind = kind
            Key = key
            InitValue = initValue
            Validator = validator
            Variation = Nothing
            Comment = None
        }
    member this.ToAlias alias =
        {this with Type = alias}
    interface IFieldMeta with
        member this.Type = this.Variation.DecorateType this.Type
        member this.Encoder = this.Variation.DecorateEncoder this.Encoder
        member this.Decoder = this.Variation.DecorateDecoder this.Decoder
        member this.Spec = this.Variation.DecorateSpec this.Encoder this.Decoder this.Spec
        member this.Key = this.Key
        member this.ToVariant variation =
            match this.Kind with
            | VarProperty ->
                {this with Variation = variation}
                :> IFieldMeta
            | _ ->
                failWith "Unsupported" <| sprintf "%A<%s>" this.Kind this.Type
        member this.Comment = this.Comment
    interface IPropMeta with
        member this.Kind = this.Kind
        member this.InitValue = this.Variation.DecorateInitValue this.InitValue
        member this.Validator = this.Validator

type ComboMeta = {
    Parents : (string * ComboMeta) list
    Fields : IPropMeta list
} with
    static member Create parents fields =
        {
            Parents = parents
            Fields = fields
        }
    member this.AddField field =
        {this with Fields = this.Fields @ [field]}

type CaseMeta = {
    Kind : string
    Fields : IFieldMeta list
} with
    static member Create kind fields =
        {
            Kind = kind
            Fields = fields
        }

type UnionFieldMeta = {
    Kind : string
    Cases : CaseMeta list
    Key : string
    Variation : FieldVariation
} with
    static member Create kind cases key =
        {
            Kind = kind
            Cases = cases
            Key = key
            Variation = Nothing
        }
    interface IFieldMeta with
        member this.Type =
            this.Variation.DecorateType this.Kind
        member this.Encoder =
            this.Kind + ".JsonEncoder"
            |> this.Variation.DecorateEncoder
        member this.Decoder =
            this.Kind + ".JsonDecoder"
            |> this.Variation.DecorateDecoder
        member this.Spec =
            this.Kind + ".JsonSpec"
            |> this.Variation.DecorateSpec (this.Kind + ".JsonEncoder") (this.Kind + ".JsonDecoder")
        member this.Key = this.Key
        member this.ToVariant variation =
            {this with Variation = variation}
            :> IFieldMeta
        member this.Comment = None

type UnionPropMeta = {
    Kind : string
    Cases : CaseMeta list
    Key : string
    InitValue : string
    Validator : string
    Variation : FieldVariation
} with
    static member Create kind cases key initValue validator =
        {
            Kind = kind
            Cases = cases
            Key = key
            InitValue = initValue
            Validator = validator
            Variation = Nothing
        }
    interface IFieldMeta with
        member this.Type =
            this.Variation.DecorateType this.Kind
        member this.Encoder =
            this.Kind + ".JsonEncoder"
            |> this.Variation.DecorateEncoder
        member this.Decoder =
            this.Kind + ".JsonDecoder"
            |> this.Variation.DecorateDecoder
        member this.Spec =
            this.Kind + ".JsonSpec"
            |> this.Variation.DecorateSpec (this.Kind + ".JsonEncoder") (this.Kind + ".JsonDecoder")
        member this.Key = this.Key
        member this.ToVariant variation =
            {this with Variation = variation}
            :> IFieldMeta
        member this.Comment = None
    interface IPropMeta with
        member __.Kind = VarProperty
        member this.InitValue = this.Variation.DecorateInitValue this.InitValue
        member this.Validator = this.Validator

[<AutoOpen>]
module Extensions =
    type IPropMeta with
        member this.EncoderCall =
            if this.Encoder = "" then
                ""
            else
                sprintf "%s " this.Encoder
        member this.PropType =
            match this.Kind with
            | VarProperty ->
                sprintf "IVarProperty<%s>" this.Type
            | _ ->
                failWith "Unsupported" <| sprintf "%A<%s>" this.Kind this.Type
        member this.CommentCode =
            this.Comment
            |> Option.map (fun comment -> sprintf "(* %s *) " comment)
            |> Option.defaultValue ""
    type ComboMeta with
        member this.AllFields =
            [
                for (_parentName, parentMeta) in this.Parents do
                    yield parentMeta.Fields
                yield this.Fields
            ] |> List.concat
            |> List.foldBack (fun (field : IPropMeta) fields ->
                fields
                |> List.exists (fun f ->
                    if f.Key = field.Key then
                        if f.PropType <> field.PropType then
                            failWith "Key_Conflicted" <| sprintf "%s <> %s" field.PropType f.PropType
                        else
                            true
                    else
                        false
                )|> function
                    | true ->
                        fields
                    | false ->
                        field :: fields
            ) []

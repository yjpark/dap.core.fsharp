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
    | Dict
with
    member this.Decorator : string option =
        match this with
        | Nothing -> None
        | Option -> Some "option"
        | List -> Some "list"
        | Dict -> Some "dict"
    member this.DecorateType type' =
        match this with
        | Dict ->
            sprintf "Map<string, %s>" type'
        | _ ->
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
        | Dict -> "Map.empty"

type IFieldMeta =
    abstract Type' : string with get
    abstract Type : string with get
    abstract Encoder : string with get
    abstract Decoder : string with get
    abstract Spec : string with get
    abstract Key : string with get
    abstract Variation : FieldVariation with get
    abstract Comment : string option with get
    abstract ToVariant0 : FieldVariation -> IFieldMeta

type IPropMeta =
    inherit IFieldMeta
    abstract Kind : PropertyKind with get
    abstract InitValue : string with get
    abstract Validator : string with get
    abstract ToVariant : FieldVariation -> IPropMeta
    abstract WithComment : string option -> IPropMeta

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
    static member CreateCombo key =
        PropMeta.Create "IComboProperty" "" "" "" PropertyKind.ComboProperty key "" ""
    member this.ToAlias alias =
        {this with Type = alias}
    member this.ToVariant variation =
        match this.Kind with
        | VarProperty ->
            {this with Variation = variation}
        | ComboProperty
        | CustomProperty ->
            match variation with
            | FieldVariation.List
            | FieldVariation.Dict ->
                {this with Variation = variation}
            | _ ->
                failWith "Unsupported" <| sprintf "%A<%s> %A" this.Kind this.Type variation
        | _ ->
            failWith "Unsupported" <| sprintf "%A<%s> %A" this.Kind this.Type variation
    interface IFieldMeta with
        member this.Type' = this.Type
        member this.Type = this.Variation.DecorateType this.Type
        member this.Encoder = this.Variation.DecorateEncoder this.Encoder
        member this.Decoder = this.Variation.DecorateDecoder this.Decoder
        member this.Spec = this.Variation.DecorateSpec this.Encoder this.Decoder this.Spec
        member this.Key = this.Key
        member this.Variation = this.Variation
        member this.Comment = this.Comment
        member this.ToVariant0 variation = this.ToVariant variation :> IFieldMeta
    interface IPropMeta with
        member this.Kind = this.Kind
        member this.InitValue = this.Variation.DecorateInitValue this.InitValue
        member this.Validator = this.Validator
        member this.ToVariant variation = this.ToVariant variation :> IPropMeta
        member this.WithComment comment = {this with Comment = comment} :> IPropMeta

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
        member this.Type' = this.Kind
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
        member this.Variation = this.Variation
        member this.Comment = None
        member this.ToVariant0 variation =
            {this with Variation = variation}
            :> IFieldMeta

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
        member this.Type' = this.Kind
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
        member this.Variation = this.Variation
        member this.Comment = None
        member this.ToVariant0 variation =
            {this with Variation = variation}
            :> IFieldMeta
    interface IPropMeta with
        member __.Kind = VarProperty
        member this.InitValue = this.Variation.DecorateInitValue this.InitValue
        member this.Validator = this.Validator
        member this.ToVariant variation =
            {this with Variation = variation}
            :> IPropMeta
        member this.WithComment _comment = this :> IPropMeta

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
                match this.Variation with
                | FieldVariation.List ->
                    sprintf "IListProperty<IVarProperty<%s>>" this.Type'
                | FieldVariation.Dict ->
                    sprintf "IDictProperty<IVarProperty<%s>>" this.Type'
                | FieldVariation.Option ->
                    sprintf "IVarProperty<%s>" this.Type
                | FieldVariation.Nothing ->
                    sprintf "IVarProperty<%s>" this.Type
            | ComboProperty ->
                match this.Variation with
                | FieldVariation.Nothing ->
                    sprintf "IComboProperty"
                | FieldVariation.List ->
                    sprintf "IListProperty<IComboProperty>"
                | FieldVariation.Dict ->
                    sprintf "IDictProperty<IComboProperty>"
                | FieldVariation.Option ->
                    failWith "Unsupported" <| sprintf "%A<%s> %A" this.Kind this.Type this.Variation
            | _ ->
                failWith "Unsupported" <| sprintf "%A<%s> %A" this.Kind this.Type this.Variation
        member this.CommentCode =
            this.Comment
            |> Option.map (fun comment -> sprintf "(* %s *) " comment)
            |> Option.defaultValue ""
    type ComboMeta with
        member this.GetAllFields' (name : string, names' : Set<string>) : Set<string> * (IPropMeta list) =
            if names' |> Set.contains name then
                (names', [])
            else
                let mutable names = names' |> Set.add name
                let mutable fields = []
                for (parentName, parentMeta) in this.Parents do
                    let (parentNames, parentFields) = parentMeta.GetAllFields' (parentName, names)
                    names <-
                        names
                        |> Set.union parentNames
                    fields <- fields @ parentFields
                (names,
                    this.Fields
                    |> List.map (fun prop -> prop.WithComment (Some name))
                    |> List.append fields
                )
        member this.GetAllFields (name : string) =
            this.GetAllFields' (name, Set.empty)
            |> snd

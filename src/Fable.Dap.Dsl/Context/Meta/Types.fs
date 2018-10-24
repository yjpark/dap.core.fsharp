[<AutoOpen>]
module Dap.Context.Meta.Types

open Dap.Prelude
open Dap.Context
open Dap.Context.Meta.Util

[<AbstractClass>]
type MetaBuilder<'obj> () =
    member this.Yield (_ : 'a) = this.Zero ()
    abstract Zero : unit -> 'obj

[<Literal>]
let NoEncoder = ""

[<Literal>]
let NoDecoder = ""

[<Literal>]
let NoSpec = ""

[<Literal>]
let NoInitValue = ""

[<Literal>]
let NoValidator = ""

[<RequireQualifiedAccess>]
type FieldType =
    | Coded of string
    | Basic of string
    | Custom of string
    | Property of string
    | Alias of FieldType * string
with
    static member CreateCoded t : FieldType = Coded t
    static member CreateBasic t : FieldType = Basic t
    static member CreateCustom t : FieldType = Custom t
    static member CreateProperty t : FieldType = Property t
    member this.ToAlias alias : FieldType =
        match this with
        | Alias (t, _a) -> t.ToAlias alias
        | _ -> Alias (this, alias)
    member this.Encoder =
        match this with
        | Coded t -> NoEncoder
        | Basic t -> sprintf "E.%s" t
        | Custom t -> sprintf "%s.JsonEncoder" t
        | Property t -> NoEncoder
        | Alias (t, _a) -> t.Encoder
    member this.Decoder =
        match this with
        | Coded t -> NoDecoder
        | Basic t -> sprintf "D.%s" t
        | Custom t -> sprintf "%s.JsonDecoder" t
        | Property t -> NoDecoder
        | Alias (t, _a) -> t.Decoder
    member this.Spec =
        match this with
        | Coded t -> NoSpec
        | Basic t -> sprintf "S.%s" t
        | Custom t -> sprintf "%s.JsonSpec" t
        | Property t -> NoSpec
        | Alias (t, _a) -> t.Spec
    member this.ValueType =
        match this with
        | Coded t -> t
        | Basic t -> t
        | Custom t -> t
        | Property t -> t
        | Alias (_t, a) -> a
    member this.PropType' (alias : string option) =
        match this with
        | Coded t -> sprintf "IVarProperty<%s>" (defaultArg alias t)
        | Basic t -> sprintf "IVarProperty<%s>" (defaultArg alias t)
        | Custom t -> sprintf "IVarProperty<%s>" (defaultArg alias t)
        | Property t -> t
        | Alias (t, a) -> t.PropType' (Some a)
    member this.PropType =
        match this with
        | Alias (t, a) -> t.PropType' (Some a)
        | _ -> this.PropType' None

[<RequireQualifiedAccess>]
type FieldVariation =
    | NoVariation
    | Option
    | List
    | Dict
with
    member this.Encoder (f : FieldType) =
        let encoder = f.Encoder
        if encoder = NoEncoder then
            NoEncoder
        else
            match this with
            | NoVariation -> encoder
            | Option -> sprintf "(E.option %s)" encoder
            | List -> sprintf "(E.list %s)" encoder
            | Dict -> sprintf "(E.dict %s)" encoder
    member this.Decoder (f : FieldType) =
        let decoder = f.Decoder
        if decoder = NoDecoder then
            NoDecoder
        else
            match this with
            | NoVariation -> decoder
            | Option -> sprintf "(D.option %s)" decoder
            | List -> sprintf "(D.list %s)" decoder
            | Dict -> sprintf "(D.dict %s)" decoder
    member this.Spec (f : FieldType) =
        let spec = f.Spec
        if spec = NoSpec then
            NoSpec
        else
            match this with
            | NoVariation -> spec
            | Option -> sprintf "(S.option (%s, %s))" f.Encoder f.Decoder
            | List -> sprintf "(S.list (%s, %s))" f.Encoder f.Decoder
            | Dict -> sprintf "(S.dict (%s, %s))" f.Encoder f.Decoder
    member this.ValueType (f : FieldType) =
        let t = f.ValueType
        match this with
        | NoVariation -> t
        | Option -> sprintf "%s option" t
        | List -> sprintf "%s list" t
        | Dict -> sprintf "Map<string, %s>" t
    member this.PropType (f : FieldType) =
        let propType = f.PropType
        match this with
        | NoVariation -> propType
        | Option ->
            let isVar = propType.StartsWith ("IVarProperty<")
            if isVar then
                sprintf "IVarProperty<%s>" <| this.ValueType f
            else
                failWith "PropType:Not_Supported" <| sprintf "<%A> %A" f this
        | List -> sprintf "IListProperty<%s>" propType
        | Dict -> sprintf "IDictProperty<%s>" propType
    member this.InitValue (_f : FieldType) (initValue : string) =
        match this with
        | NoVariation -> initValue
        | Option -> "None"
        | List -> "[]"
        | Dict -> "Map.empty"

type FieldMeta = {
    Type : FieldType
    Key : Key
    Value : string
    Validator : string option
    Variation : FieldVariation
    Comment : string option
} with
    static member Create' t key value validator : FieldMeta =
        {
            Type = t
            Key = key
            Value = value
            Validator = validator
            Variation = FieldVariation.NoVariation
            Comment = None
        }
    static member CreateCoded t key value validator : FieldMeta =
            FieldMeta.Create' (FieldType.CreateCoded t) key value validator
    static member CreateBasic t key value validator : FieldMeta =
            FieldMeta.Create' (FieldType.CreateBasic t) key value validator
    static member CreateCustom t key value validator : FieldMeta =
            FieldMeta.Create' (FieldType.CreateCustom t) key value validator
    static member CreateProperty t key value validator : FieldMeta =
            FieldMeta.Create' (FieldType.CreateProperty t) key value validator
    member this.AsString =
        sprintf "<%A> %A [%s]" this.Type this.Variation this.Key
    member this.ToAlias alias =
        {this with Type = this.Type.ToAlias alias}
    member this.ToVariant variation =
        match this.Variation with
        | FieldVariation.NoVariation ->
            {this with Variation = variation}
        | _ ->
            failWith "ToVariant" <| sprintf "%s -> %A" this.AsString variation
    member this.WithValue (value : string) =
        {this with Value = value}
    member this.WithValue (value : Json) =
    #if FABLE_COMPILER
        // this can't be called in Fable generators, since encoder calls js code
        failWith "Fable:WithValue" <| sprintf "%s -> %s" this.AsString ^<| value.ToString ()
    #else
        let json = E.encode 0 value
        let decoder = this.Decoder
        if decoder = NoDecoder then
            failWith "WithValue" <| sprintf "%s -> %s" this.AsString json
        else
            if json.StartsWith "\"" then
                sprintf "(decodeJsonString %s \"\"%s\"\")" this.Decoder json
            else
                sprintf "(decodeJsonValue %s \"\"\"%s\"\"\")" this.Decoder json
            |> fun value -> {this with Value = value}
    #endif
    member this.WithValidator validator =
        {this with Validator = validator}
    member this.WithComment comment =
        {this with Comment = comment}
    member this.Encoder = this.Variation.Encoder this.Type
    member this.Decoder = this.Variation.Decoder this.Type
    member this.Spec = this.Variation.Spec this.Type
    member this.ValueType = this.Variation.ValueType this.Type
    member this.PropType = this.Variation.PropType this.Type
    member this.InitValue = this.Variation.InitValue this.Type this.Value
    member this.CommentCode =
        this.Comment
        |> Option.map (fun comment -> sprintf "(* %s *) " comment)
        |> Option.defaultValue ""
    static member SetFallbackComment (name : string) (this : FieldMeta) =
        match this.Comment with
        | Some _ -> this
        | None -> this.WithComment (Some name)
    member this.EncoderCall =
        if this.Encoder = NoEncoder then
            failWith "EncoderCall" this.AsString
        else
            sprintf "%s " this.Encoder
    member this.AddPropCode =
        let validator =
            match this.Validator with
            | None ->
                "None"
            | Some validator ->
                if validator = NoValidator then
                    "None"
                else
                    sprintf "Some %s" validator
        let propType = this.Type.PropType
        let isVar = propType.StartsWith ("IVarProperty<")
        let isCombo = propType = "IComboProperty"
        match this.Variation with
        | FieldVariation.NoVariation ->
            if isVar then
                sprintf "AddVar<%s%s> (%s, %s, \"%s\", %s, %s)"
                    this.CommentCode this.Type.ValueType this.Type.Encoder this.Type.Decoder this.Key this.Value validator
            elif isCombo then
                sprintf "AddCombo%s (\"%s\")"
                    this.CommentCode this.Key
            else
                sprintf "AddCustom<%s%s> (%s.Create, \"%s\")"
                    this.CommentCode propType propType this.Key
        | FieldVariation.Option ->
            if isVar then
                sprintf "AddVar<%s%s> (%s, %s, \"%s\", %s, %s)"
                    this.CommentCode this.ValueType this.Encoder this.Decoder this.Key this.InitValue "None" //TODO: Convert validator properly
            else
                failWith "AddPropCode" this.AsString
        | FieldVariation.List ->
            if isVar then
                sprintf "AddList<%s%s> (%s, %s, \"%s\", %s, %s)"
                    this.CommentCode this.Type.ValueType this.Type.Encoder this.Type.Decoder this.Key this.Value validator
            else
                sprintf "AddList<%s%s> (%s.Create, \"%s\")"
                    this.CommentCode propType propType this.Key
        | FieldVariation.Dict ->
            if isVar then
                sprintf "AddDict<%s%s> (%s, %s, \"%s\", %s, %s)"
                    this.CommentCode this.Type.ValueType this.Type.Encoder this.Type.Decoder this.Key this.Value validator
            else
                sprintf "AddDict<%s%s> (%s.Create, \"%s\")"
                    this.CommentCode propType propType this.Key

type ICustomMeta =
    abstract GetInitValue : string -> string

let private convertInitValue (name : string) (initValue : string option) =
    initValue
    |> Option.map (fun initValue ->
        if initValue.StartsWith "."
            || initValue.StartsWith "(." then
            sprintf "%s%s" name initValue
        else
            initValue
    )

type ComboMeta = {
    Parents : (string * ComboMeta) list
    Fields : FieldMeta list
    InitValue : string option
} with
    static member Create parents fields =
        {
            Parents = parents
            Fields = fields
            InitValue = None
        }
    static member SetInitValue initValue (this : ComboMeta) =
        {this with InitValue = initValue}
    member this.WithInitValue initValue =
        this |> ComboMeta.SetInitValue initValue
    member this.AddField field =
        {this with Fields = this.Fields @ [field]}
    member this.GetAllFields' (name : string, names' : Set<string>) : Set<string> * (FieldMeta list) =
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
                |> List.map ^<| FieldMeta.SetFallbackComment name
                |> List.append fields
            )
    member this.GetAllFields (name : string) =
        this.GetAllFields' (name, Set.empty)
        |> snd
    member this.HasDefault' (fields : FieldMeta list) =
        let noDefault =
            fields
            |> List.exists (fun f -> f.InitValue = NoInitValue)
        not noDefault
    member this.HasDefault (name : string) =
        this.HasDefault' <| this.GetAllFields name
    interface ICustomMeta with
        member this.GetInitValue (name : string) =
            this.InitValue
            |> convertInitValue name
            |> Option.defaultWith (fun () ->
                if this.HasDefault name then
                    sprintf "(%s.Default ())" name
                else
                    NoInitValue
            )

type CaseMeta = {
    Kind : string
    Fields : FieldMeta list
} with
    static member Create kind fields =
        {
            Kind = kind
            Fields = fields
        }

type UnionMeta = {
    Cases : CaseMeta list
    InitValue : string option
} with
    static member Create cases =
        {
            Cases = cases
            InitValue = None
        }
    static member SetInitValue initValue (this : UnionMeta) =
        {this with InitValue = initValue}
    member this.WithInitValue initValue =
        this |> UnionMeta.SetInitValue initValue
    member this.AddCase case =
        {this with Cases = this.Cases @ [case]}
    interface ICustomMeta with
        member this.GetInitValue (name : string) =
            this.InitValue
            |> convertInitValue name
            |> Option.defaultValue NoInitValue

type ChannelMeta = {
    Evt : FieldMeta
} with
    static member Create evt =
        {
            Evt = evt
        }

type HandlerMeta = {
    Req : FieldMeta
    Res : FieldMeta
    Handler : string list
} with
    static member Create req res =
        {
            Req = req
            Res = res
            Handler = []
        }

type ContextMeta = {
    Kind : Kind option
    Properties : string * ComboMeta
    Channels : ChannelMeta list
    Handlers : HandlerMeta list
} with
    static member Create properties =
        {
            Kind = None
            Properties = properties
            Channels = []
            Handlers = []
        }
    member this.AddChannel channel =
        {this with Channels = this.Channels @ [channel]}
    member this.AddHandler handler =
        {this with Handlers = this.Handlers @ [handler]}
    member this.IsAbstract =
        this.Handlers.Length > 0

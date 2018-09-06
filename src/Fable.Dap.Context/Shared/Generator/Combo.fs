[<RequireQualifiedAccess>]
module Dap.Context.Generator.Combo

open System.Reflection

open Dap.Prelude
open Dap.Context
open Dap.Context.Internal
open Dap.Context.Generator.Util

let internal getValueType (prop : IProperty) =
    match prop with
    | :? IVarProperty as prop ->
        match prop.Spec.Kind with
        | PK_Bool -> "bool"
        | PK_Int -> "int"
        | PK_String -> "string"
        | PK_Float -> "float"
        | PK_Decimal -> "decimal"
        | PK_Long -> "int64"
        | _ ->
            prop.ValueType.FullName
    | :? IComboProperty ->
        "IComboProperty"
    | _ ->
        prop.Spec.Kind

let internal getEncoder (prop : IProperty) =
    match prop with
    | :? IVarProperty as prop ->
        match prop.Spec.Kind with
        | PK_Bool -> "E.bool"
        | PK_Int -> "E.int"
        | PK_String -> "E.string"
        | PK_Float -> "E.float"
        | PK_Decimal -> "E.decimal"
        | PK_Long -> "E.long"
        | _ ->
            prop.ValueType.FullName
    | :? IComboProperty ->
        "E.object"
    | _ ->
        sprintf "%s.JsonEncoder" prop.Spec.Kind

let internal getDecoder (prop : IProperty) =
    match prop with
    | :? IVarProperty as prop ->
        match prop.Spec.Kind with
        | PK_Bool -> "D.bool"
        | PK_Int -> "D.int"
        | PK_String -> "D.string"
        | PK_Float -> "D.float"
        | PK_Decimal -> "D.decimal"
        | PK_Long -> "D.long"
        | _ ->
            prop.ValueType.FullName
    | :? IComboProperty ->
        "D.object"
    | _ ->
        sprintf "%s.JsonDecoder" prop.Spec.Kind

let internal getSpec (prop : IProperty) =
    match prop with
    | :? IVarProperty as prop ->
        match prop.Spec.Kind with
        | PK_Bool -> "S.bool"
        | PK_Int -> "S.int"
        | PK_String -> "S.string"
        | PK_Float -> "S.float"
        | PK_Decimal -> "S.decimal"
        | PK_Long -> "S.long"
        | _ ->
            prop.ValueType.FullName
    | _ ->
        sprintf "%s.JsonSpec" prop.Spec.Kind

let internal getInitValue (prop : IProperty) =
    E.encode 0 prop.Spec.InitValue

let internal getPropType (prop : IProperty) =
    match prop with
    | :? IVarProperty as prop ->
        sprintf "IVarProperty<%s>" <| getValueType prop
    | :? IComboProperty ->
        "IComboProperty"
    | _ -> "N/A"

type InterfaceGenerator (template : IComboProperty) =
    let getInterfaceHeader (param : InterfaceParam) =
        [
            yield sprintf "type %s =" param.Name
        ]
    let getOperation (param : InterfaceParam) (prop : IProperty) =
        [
            match param.Type with
            | ComboInterface -> getPropType prop
            | ValueInterface -> getValueType prop
            |> sprintf "    abstract %s : %s with get" prop.Spec.Key.AsCamelCase
        ]
    static member GetImplementation (face : Interface) =
        [
            yield sprintf "    interface %s with" face.Param.Name
            match face.Template with
            | :? IComboProperty as combo ->
                for prop in combo.ValueAsList do
                    let name = prop.Spec.Key.AsCamelCase
                    yield sprintf "        member this.%s = this.%s" name name
            | _ ->
                yield sprintf "        //Unsupported interface template: %s" <| (face.Template.GetType()) .FullName
        ]
    static member GetImplementations (interfaces : Interface list) =
        interfaces
        |> List.map InterfaceGenerator.GetImplementation
        |> List.concat
    interface IGenerator<InterfaceParam> with
        member __.Generate param =
            let fields = template.ValueAsList
            [
                getInterfaceHeader param
                fields
                |> List.map ^<| getOperation param
                |> List.concat
            ]|> List.concat

type RecordGenerator (template : IComboProperty) =
    let getRecordHeader (param : RecordParam) =
        [
            yield sprintf "type %s = {" param.Name
        ]
    let getJsonEncoder (param : RecordParam) (fields : IProperty list) =
        [
            yield sprintf "    static member JsonEncoder : JsonEncoder<%s> =" param.Name
            yield sprintf "        fun (this : %s) ->" param.Name
            yield sprintf "            E.object ["
            for prop in fields do
                yield sprintf "                \"%s\", %s this.%s" prop.Spec.Key (getEncoder prop) prop.Spec.Key.AsCamelCase
            yield sprintf "            ]"
        ]
    let getJsonDecoder (param : RecordParam) (fields : IProperty list) =
        [
            yield sprintf "    static member JsonDecoder : JsonDecoder<%s> =" param.Name
            yield sprintf "        D.decode %s.Create" param.Name
            for prop in fields do
                if param.IsLoose then
                    yield sprintf "        |> D.optional \"%s\" %s %s" prop.Spec.Key (getDecoder prop) (getInitValue prop)
                else
                    yield sprintf "        |> D.required \"%s\" %s" prop.Spec.Key (getDecoder prop)
        ]
    let getRecordMiddle (param : RecordParam) (fields : IProperty list) =
        let keys =
            fields
            |> List.map (fun f -> f.Spec.Key)
            |> String.concat " "
        let initValues =
            fields
            |> List.map getInitValue
            |> String.concat " "
        [
            yield sprintf "} with"
            yield sprintf "    static member Create %s" keys
            yield sprintf "            : %s =" param.Name
            yield sprintf "        {"
            for field in fields do
                yield sprintf "            %s = %s" field.Spec.Key.AsCamelCase field.Spec.Key
            yield sprintf "        }"
            yield sprintf "    static member Default () ="
            yield sprintf "        %s.Create %s" param.Name initValues
        ] @ (
            if param.IsJson then
                [
                    sprintf "    static member JsonSpec ="
                    sprintf "        FieldSpec.Create<%s>" param.Name
                    sprintf "            %s.JsonEncoder %s.JsonDecoder" param.Name param.Name
                ] @ getJsonEncoder param fields
                @ getJsonDecoder param fields
                @ [
                    sprintf "    interface IJson with"
                    sprintf "        member this.ToJson () = %s.JsonEncoder this" param.Name
                ]
            else
                []
        )
    let getFieldAdder (prop : IProperty) =
        let spec = prop.Spec
        sprintf "    %s : %s" spec.Key.AsCamelCase (getValueType prop)
    let getFieldMember (prop : IProperty) =
        let spec = prop.Spec
        let name = toCamelCase spec.Key
        sprintf "    member this.With%s (%s : %s) = {this with %s = %s}" name spec.Key (getValueType prop) name spec.Key
    interface IGenerator<RecordParam> with
        member __.Generate param =
            let fields = template.ValueAsList
            [
                getRecordHeader param
                fields |> List.map getFieldAdder
                getRecordMiddle param fields
                fields |> List.map getFieldMember
                InterfaceGenerator.GetImplementations param.Interfaces
            ]|> List.concat

type ClassGenerator (template : IComboProperty) =
    let getClassHeader (param : ClassParam) =
        [
            if param.IsAbstract then
                yield "[<AbstractClass>]"
            yield sprintf "type %s (owner : IOwner, key : Key) =" param.Name
            yield sprintf "    inherit WrapProperties<%s, IComboProperty> (\"%s\")" param.Name param.Kind
            yield sprintf "    let target = Properties.combo owner key"
        ]
    let getClassMiddle (param : ClassParam) =
        [
            yield sprintf "    do ("
            if param.IsFinal then
                yield sprintf "        target.SealCombo ()"
            yield sprintf "        base.Setup (target)"
            yield sprintf "    )"
            yield sprintf "    static member Create o k = new %s (o, k)" param.Name
            yield sprintf "    static member Empty () = %s.Create noOwner NoKey" param.Name
            yield sprintf "    override this.Self = this"
            yield sprintf "    override __.Spawn o k = %s.Create o k" param.Name
            yield sprintf "    override __.SyncTo t = target.SyncTo t.Target"
        ]
    let getFieldAdder (prop : IProperty) =
        let spec = prop.Spec
        let initValue = getInitValue prop
        let validator =
            prop.Spec.ValidatorKind
            |> Option.map (fun k -> sprintf "(Some %s)" k)
            |> Option.defaultValue "None"
        sprintf "    let %s = target.Add%s (\"%s\", %s, %s)" spec.Key spec.Kind spec.Key initValue validator
    let getFieldMember (prop : IProperty) =
        let spec = prop.Spec
        sprintf "    member __.%s : %s = %s" (toCamelCase spec.Key) (getPropType prop) spec.Key
    interface IGenerator<ClassParam> with
        member __.Generate param =
            let fields =
                template.Value
                |> Map.toList
                |> List.map snd
            [
                getClassHeader param
                fields |> List.map getFieldAdder
                getClassMiddle param
                fields |> List.map getFieldMember
                InterfaceGenerator.GetImplementations param.Interfaces
            ]|> List.concat

type BuilderGenerator (template : IComboProperty) =
    let getBuilderHeader (param : BuilderParam) =
        [
            yield sprintf "type %s () =" param.Name
            yield sprintf "    inherit ObjBuilder<%s> ()" param.Kind
            yield sprintf "    override __.Zero () = %s.Empty ()" param.Kind
        ]
    let getBuilderFooter (param : BuilderParam) =
        [
            yield sprintf ""
            yield sprintf "let %s = %s ()" param.Key param.Name
        ]
    let getOperation (param : BuilderParam) (prop : IProperty) =
        let spec = prop.Spec
        let name = toCamelCase spec.Key
        [
            sprintf "    [<CustomOperation(\"%s\")>]" spec.Key
            sprintf "    member __.%s (target : %s, v) =" spec.Key.AsCamelCase param.Kind
            sprintf "        target.%s.SetValue v |> ignore" spec.Key.AsCamelCase
            sprintf "        target"
        ]
    interface IGenerator<BuilderParam> with
        member __.Generate param =
            let fields =
                template.Value
                |> Map.toList
                |> List.map snd
            [
                getBuilderHeader param
                fields
                |> List.map ^<| getOperation param
                |> List.concat
                getBuilderFooter param
            ]|> List.concat


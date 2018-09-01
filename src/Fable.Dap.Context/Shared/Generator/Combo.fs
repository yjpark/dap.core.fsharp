[<RequireQualifiedAccess>]
module Dap.Context.Generator.Combo

open System.Reflection

open Dap.Prelude
open Dap.Context
open Dap.Context.Internal
open Dap.Context.Generator.Util

let getValueType (prop : IProperty) =
    match prop with
    | :? IVarProperty as prop ->
        match prop.Spec.Kind with
        | PK_Bool -> "bool"
        | PK_Int -> "int"
        | PK_Long -> "int64"
        | PK_Float -> "float"
        | PK_Double -> "double"
        | PK_String -> "string"
        | _ ->
            prop.ValueType.FullName
    | :? IComboProperty ->
        "IComboProperty"
    | _ -> "N/A"

type RecordGenerator (template : IComboProperty) =
    let getRecordHeader (param : RecordParam) =
        [
            yield sprintf "type %s = {" param.Name
        ]
    let getRecordMiddle (param : RecordParam) (fields : IProperty list) =
        let keys =
            fields
            |> List.map (fun f -> f.Spec.Key)
            |> String.concat " "
        let initValues =
            fields
            |> List.map (fun f -> E.encode 0 f.Spec.InitValue)
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
        ]
    let getFieldAdder (prop : IProperty) =
        let spec = prop.Spec
        sprintf "    %s : %s" spec.Key.AsCamelCase (getValueType prop)
    let getFieldMember (prop : IProperty) =
        let spec = prop.Spec
        let name = toCamelCase spec.Key
        sprintf "    member this.With%s %s = {this with %s = %s}" name spec.Key name spec.Key
    interface IRecordGenerator with
        member __.Generate param =
            let fields =
                template.Value
                |> Map.toList
                |> List.map snd
            [
                getRecordHeader param
                fields |> List.map getFieldAdder
                getRecordMiddle param fields
                fields |> List.map getFieldMember
            ]|> List.reduce (@)

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
        let initValue = E.encode 0 spec.InitValue
        let validator =
            prop.Spec.ValidatorKind
            |> Option.map (fun k -> sprintf "(Some %s)" k)
            |> Option.defaultValue "None"
        sprintf "    let %s = target.Add%s \"%s\" %s %s" spec.Key spec.Kind spec.Key initValue validator
    let getFieldMember (prop : IProperty) =
        let spec = prop.Spec
        sprintf "    member __.%s = %s" (toCamelCase spec.Key) spec.Key
    interface IClassGenerator with
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
            ]|> List.reduce (@)
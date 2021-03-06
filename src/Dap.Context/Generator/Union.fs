[<RequireQualifiedAccess>]
module Dap.Context.Generator.Union

open System.Reflection

open Dap.Prelude
open Dap.Context
open Dap.Context.Meta
open Dap.Context.Generator.Util

type UnionGenerator (meta : UnionMeta) =
    let getUnionHeader (param : UnionParam) =
        [
            yield sprintf "type %s =" param.Name
        ]
    let getCaseSpec (param : UnionParam) (case : CaseMeta) =
        if case.Fields.Length = 0 then
            [
                sprintf "            CaseSpec<%s>.Create (\"%s\", [])" param.Name case.Kind
            ]
        else
            [
                sprintf "            CaseSpec<%s>.Create (\"%s\", [" param.Name case.Kind
                case.Fields
                |> List.map (fun f -> f.Spec)
                |> String.concat " ; "
                |> sprintf "                %s"
                sprintf "            ])"

            ]
    let getCaseCreate (param : UnionParam) (case : CaseMeta) =
        let names =
            if case.Fields.Length = 0 then
                "()"
            else
                case.Fields
                |> List.map (fun f -> f.Key.AsCodeVariableName)
                |> String.concat " "
        let values =
            if case.Fields.Length = 0 then
                ""
            else
                case.Fields
                |> List.map (fun f -> f.Key.AsCodeVariableName)
                |> String.concat ", "
                |> sprintf " (%s)"
        let createKind = if meta.Cases.Length = 1 then "" else case.Kind.AsCodeMemberName
        [
            sprintf "    static member Create%s %s : %s =" createKind names param.Name
            sprintf "        %s%s" case.Kind.AsCodeMemberName values
        ]
    let getUnionMiddle (param : UnionParam) =
        [
            sprintf "with"
        ] @ (
            meta.Cases
            |> List.map ^<| getCaseCreate param
            |> List.concat
        ) @ (
            if param.IsJson then
                [
                    sprintf "    static member JsonSpec' : CaseSpec<%s> list =" param.Name
                    sprintf "        ["
                ] @ (meta.Cases |> List.map (getCaseSpec param) |> List.concat)
                @ [
                    sprintf "        ]"
                    sprintf "    static member JsonEncoder = E.union %s.JsonSpec'" param.Name
                    sprintf "    static member JsonDecoder = D.union %s.JsonSpec'" param.Name
                    sprintf "    static member JsonSpec ="
                    sprintf "        FieldSpec.Create<%s> (%s.JsonEncoder, %s.JsonDecoder)" param.Name param.Name param.Name
                    sprintf "    interface IJson with"
                    sprintf "        member this.ToJson () = %s.JsonEncoder this" param.Name
                ]
            else
                []
        )
    let getFieldDefinition (field : FieldMeta) =
        sprintf "%s : %s" field.Key.AsCodeVariableName field.ValueType
    let getCaseDefinition (case : CaseMeta) =
        let fields =
            if case.Fields.Length = 0 then
                ""
            else
                case.Fields
                |> List.map getFieldDefinition
                |> String.concat " * "
                |> sprintf " of %s"
        sprintf "    | %s%s" case.Kind.AsCodeMemberName fields
    interface IGenerator<UnionParam> with
        member __.Generate param =
            [
                getUnionHeader param
                meta.Cases |> List.map getCaseDefinition
                getUnionMiddle param
            ]|> List.concat

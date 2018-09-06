[<RequireQualifiedAccess>]
module Dap.Context.Generator.Union

open System.Reflection

open Dap.Prelude
open Dap.Context
module Union = Dap.Context.Builder.Union

type UnionGenerator (template : IListProperty<Union.CaseProperty>) =
    let getUnionHeader (param : UnionParam) =
        [
            yield sprintf "type %s =" param.Name
        ]
    let getCaseSpec (param : UnionParam) (case : Union.CaseProperty) =
        if case.Fields.Count = 0 then
            [
                sprintf "            CaseSpec<%s>.Create \"%s\" []" param.Name case.Kind.Value
            ]
        else
            [
                sprintf "            CaseSpec<%s>.Create \"%s\" [" param.Name case.Kind.Value
                case.Fields.ValueAsList
                |> List.map Combo.getSpec
                |> String.concat " ; "
                |> sprintf "                %s"
                sprintf "            ]"

            ]
    let getUnionMiddle (param : UnionParam) =
        [
        ] @ (
            if param.IsJson then
                [
                    sprintf "    static member JsonSpec : CaseSpec<%s> list =" param.Name
                    sprintf "        ["
                ] @ (template.Value |> List.map (getCaseSpec param) |> List.concat)
                @ [
                    sprintf "        ]"
                    sprintf "    static member JsonEncoder = E.union %s.JsonSpec" param.Name
                    sprintf "    static member JsonDecoder = D.union %s.JsonSpec" param.Name
                    sprintf "    interface IJson with"
                    sprintf "        member this.ToJson () = %s.JsonEncoder this" param.Name
                ]
            else
                []
        )
    let getFieldDefinition (field : IProperty) =
        sprintf "%s : %s" field.Spec.Key <| Combo.getValueType field
    let getCaseDefinition (case : Union.CaseProperty) =
        let fields =
            if case.Fields.Count = 0 then
                ""
            else
                case.Fields.ValueAsList
                |> List.map getFieldDefinition
                |> String.concat " * "
                |> sprintf " of %s"
        sprintf "    | %s%s" case.Kind.Value fields
    interface IGenerator<UnionParam> with
        member __.Generate param =
            [
                getUnionHeader param
                template.Value |> List.map getCaseDefinition
                getUnionMiddle param
            ]|> List.concat

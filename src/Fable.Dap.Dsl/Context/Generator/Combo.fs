[<RequireQualifiedAccess>]
module Dap.Context.Generator.Combo

open System.Reflection

open Dap.Prelude
open Dap.Context
open Dap.Context.Meta
open Dap.Context.Generator.Util
open Dap.Context.Internal

type InterfaceGenerator (meta : ComboMeta) =
    let getInterfaceHeader (param : InterfaceParam) =
        [
            yield sprintf "type %s =" param.Name
            for (parentName, parentMeta) in meta.Parents do
                yield sprintf "inherit %s" parentName
        ]
    let getOperation (param : InterfaceParam) (prop : IPropMeta) =
        [
            match param.Type with
            | ComboInterface -> prop.PropType
            | ValueInterface -> prop.Type
            |> sprintf "    abstract %s : %s with get" prop.Key.AsCodeMemberName
        ]
    static member GetImplementation (face : Interface) =
        [
            match face.Meta with
            | :? ComboMeta as meta ->
                yield sprintf "    interface %s with" face.Param.Name
                for prop in meta.Fields do
                    let name = prop.Key.AsCodeMemberName
                    yield sprintf "        member this.%s = this.%s" name name
                for (parentName, parentMeta) in meta.Parents do
                    yield sprintf "    interface %s with" parentName
                    for prop in parentMeta.Fields do
                        let name = prop.Key.AsCodeMemberName
                        yield sprintf "        member this.%s = this.%s" name name
            | _ ->
                yield sprintf "        //Unsupported interface meta: %s" <| (face.Meta.GetType()) .FullName
        ]
    static member GetImplementations (interfaces : Interface list) =
        interfaces
        |> List.map InterfaceGenerator.GetImplementation
        |> List.concat
    interface IGenerator<InterfaceParam> with
        member __.Generate param =
            [
                getInterfaceHeader param
                meta.Fields
                |> List.map ^<| getOperation param
                |> List.concat
            ]|> List.concat

type RecordGenerator (meta : ComboMeta) =
    let getRecordHeader (param : RecordParam) =
        [
            yield sprintf "type %s = {" param.Name
        ]
    let getJsonEncoder (param : RecordParam) =
        let isEmpty =
            meta.Fields
            |> List.exists (fun prop -> prop.Encoder <> "")
        [
            yield sprintf "    static member JsonEncoder : JsonEncoder<%s> =" param.Name
            yield sprintf "        fun (this : %s) ->" param.Name
            if isEmpty then
                yield sprintf "            E.object []"
            else
                yield sprintf "            E.object ["
                for prop in meta.Fields do
                    if prop.Encoder <> "" then
                        yield sprintf "                \"%s\", %sthis.%s" prop.Key prop.EncoderCall prop.Key.AsCodeMemberName
                yield sprintf "            ]"
        ]
    let getJsonDecoder (param : RecordParam) =
        [
            yield sprintf "    static member JsonDecoder : JsonDecoder<%s> =" param.Name
            yield sprintf "        D.decode %s.Create" param.Name
            for prop in meta.Fields do
                if prop.Decoder = "" then
                    yield sprintf "        |> D.hardcoded %s" prop.InitValue
                elif param.IsLoose then
                    yield sprintf "        |> D.optional \"%s\" %s %s" prop.Key prop.Decoder prop.InitValue
                else
                    yield sprintf "        |> D.required \"%s\" %s" prop.Key prop.Decoder
        ]
    let getRecordMiddle (param : RecordParam) =
        let names =
            meta.Fields
            |> List.map (fun f -> f.Key.AsCodeVariableName)
            |> String.concat " "
        let noDefault =
            meta.Fields
            |> List.exists (fun f -> f.InitValue = "")
        [
            yield sprintf "} with"
            yield sprintf "    static member Create %s" names
            yield sprintf "            : %s =" param.Name
            yield sprintf "        {"
            for field in meta.Fields do
                yield sprintf "            %s = %s" field.Key.AsCodeMemberName field.Key.AsCodeVariableName
            yield sprintf "        }"
            if not noDefault then
                yield sprintf "    static member Default () ="
                yield sprintf "        %s.Create" param.Name
                for f in meta.Fields do
                    yield sprintf "            %s" f.InitValue
        ] @ (
            if param.IsJson then
                getJsonEncoder param
                @ getJsonDecoder param
                @ [
                    sprintf "    static member JsonSpec ="
                    sprintf "        FieldSpec.Create<%s>" param.Name
                    sprintf "            %s.JsonEncoder %s.JsonDecoder" param.Name param.Name
                    sprintf "    interface IJson with"
                    sprintf "        member this.ToJson () = %s.JsonEncoder this" param.Name
                    sprintf "    interface IObj"
                ]
            else
                []
        )
    let getFieldAdder (prop : IPropMeta) =
        sprintf "    %s : %s%s" prop.Key.AsCodeMemberName prop.CommentCode prop.Type
    let getFieldMember (prop : IPropMeta) =
        [
            if prop.Decoder <> "" then
                let memberName = prop.Key.AsCodeMemberName
                let varName = prop.Key.AsCodeVariableName
                yield sprintf "    member this.With%s (%s%s : %s) = {this with %s = %s}" memberName prop.CommentCode varName prop.Type memberName varName
        ]
    interface IGenerator<RecordParam> with
        member __.Generate param =
            [
                getRecordHeader param
                meta.AllFields |> List.map getFieldAdder
                getRecordMiddle param
                meta.AllFields |> List.map getFieldMember |> List.concat
                InterfaceGenerator.GetImplementations param.Interfaces
            ]|> List.concat

type ClassGenerator (meta : ComboMeta) =
    let getClassHeader (param : ClassParam) =
        [
            if param.IsAbstract then
                yield "[<AbstractClass>]"
            yield sprintf "type %s (owner : IOwner, key : Key) =" param.Name
            yield sprintf "    inherit WrapProperties<%s, IComboProperty> ()" param.Name
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
            yield sprintf "    static member Default () = %s.Create noOwner NoKey" param.Name
            yield sprintf "    static member AddToCombo key (combo : IComboProperty) ="
            yield sprintf "        combo.AddCustom<%s>(%s.Create, key)" param.Name param.Name
            yield sprintf "    override this.Self = this"
            yield sprintf "    override __.Spawn o k = %s.Create o k" param.Name
            yield sprintf "    override __.SyncTo t = target.SyncTo t.Target"
        ]
    let getFieldAdder (prop : IPropMeta) =
        let validator =
            if prop.Validator = "" then
                "None"
            else
                sprintf "Some %s" prop.Validator
        match prop.Kind with
        | VarProperty ->
            sprintf "    let %s = target.AddVar<%s%s> (%s, %s, \"%s\", %s, %s)"
                prop.Key prop.CommentCode prop.Type prop.Encoder prop.Decoder prop.Key prop.InitValue validator
        | _ ->
            failWith "Unsupported" <| sprintf "%A<%s>" prop.Kind prop.Type

    let getFieldMember (prop : IPropMeta) =
        sprintf "    member __.%s : %s%s = %s"
            prop.Key.AsCodeMemberName prop.CommentCode prop.PropType prop.Key
    interface IGenerator<ClassParam> with
        member __.Generate param =
            [
                getClassHeader param
                meta.AllFields |> List.map getFieldAdder
                getClassMiddle param
                meta.AllFields |> List.map getFieldMember
                InterfaceGenerator.GetImplementations param.Interfaces
            ]|> List.concat

type BuilderGenerator (meta : ComboMeta) =
    let getBuilderHeader (param : BuilderParam) =
        [
            yield sprintf "type %s () =" param.Name
            yield sprintf "    inherit ObjBuilder<%s> ()" param.Kind
            yield sprintf "    override __.Zero () = %s.Default ()" param.Kind
        ]
    let getBuilderFooter (param : BuilderParam) =
        [
            yield sprintf ""
            yield sprintf "let %s = %s ()" param.Key param.Name
        ]
    let getOperation (param : BuilderParam) (prop : IPropMeta) =
        [
            if prop.Decoder <> "" then
                let memberName = prop.Key.AsCodeMemberName
                let varName = prop.Key.AsCodeVariableName
                yield sprintf "    [<CustomOperation(\"%s\")>]" prop.Key
                yield sprintf "    member __.%s (target : %s, %s%s : %s) =" memberName param.Kind prop.CommentCode varName prop.Type
                match param.Type with
                | ComboBuilder ->
                    yield sprintf "        target.%s.SetValue %s" memberName varName
                    yield sprintf "        target"
                | ValueBuilder ->
                    yield sprintf "        target.With%s %s" memberName varName
        ]
    interface IGenerator<BuilderParam> with
        member __.Generate param =
            [
                getBuilderHeader param
                meta.AllFields
                |> List.map ^<| getOperation param
                |> List.concat
                getBuilderFooter param
            ]|> List.concat


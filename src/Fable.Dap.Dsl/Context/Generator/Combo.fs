[<RequireQualifiedAccess>]
module Dap.Context.Generator.Combo

open System.Reflection

open Dap.Prelude
open Dap.Context
open Dap.Context.Meta
open Dap.Context.Generator.Util
open Dap.Context.Internal

let getAsInterfaceName (iName : string) =
    if iName.StartsWith ("I") then
        iName.Substring (1, iName.Length - 1)
    else
        iName
    |> sprintf "As%s"

let mutable private processedInterfaces : Set<string> = Set.empty
let clearProcessedInterfaces () =
    processedInterfaces <- Set.empty
let markInterfaceProcessed (pack : string) =
    processedInterfaces <- processedInterfaces |> Set.add pack
let didInterfaceProcessed (pack : string) =
    processedInterfaces |> Set.contains pack

type InterfaceGenerator (meta : ComboMeta) =
    let getInterfaceHeader (param : InterfaceParam) =
        [
            yield sprintf "type %s =" param.Name
            for (parentName) in param.Parents do
                yield sprintf "    inherit %s" parentName
            for (parentName, parentMeta) in meta.Parents do
                yield sprintf "    inherit %s" parentName
        ]
    let getOperation (param : InterfaceParam) (prop : FieldMeta) =
        [
            match param.Type with
            | ComboInterface -> prop.PropType
            | ValueInterface -> prop.ValueType
            |> sprintf "    abstract %s : %s with get" prop.Key.AsCodeMemberName
        ]
    static member GetImplementation (interfaceType : InterfaceType) ((iName, iMeta) : string * ComboMeta) =
        if didInterfaceProcessed iName then
            []
        else
            markInterfaceProcessed iName
            (
                iMeta.Parents
                |> List.map (InterfaceGenerator.GetImplementation interfaceType)
                |> List.concat
            ) @ [
                if iMeta.Fields.Length > 0 then
                    yield sprintf "    interface %s with" iName
                else
                    yield sprintf "    interface %s" iName
            ] @ (
                iMeta.Fields
                |> List.map (fun prop ->
                    let name = prop.Key.AsCodeMemberName
                    let type' =
                        match interfaceType with
                        | ComboInterface -> prop.PropType
                        | ValueInterface -> prop.ValueType
                    sprintf "        member this.%s (* %s *) : %s = this.%s" name iName type' name
                )
            ) @ [
                sprintf "    member this.%s = this :> %s" (getAsInterfaceName iName) iName
            ]
    static member GetImplementations (interfaceType : InterfaceType) (interfaces : (string * ComboMeta) list) =
        clearProcessedInterfaces ()
        interfaces
        |> List.map (InterfaceGenerator.GetImplementation interfaceType)
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
            if param.InGroup then
                yield sprintf "and %s = {" param.Name
            else
                yield sprintf "type %s = {" param.Name
        ]
    let getJsonEncoder (param : RecordParam) =
        let fields = meta.GetAllFields param.Name
        let isNotEmpty =
            fields
            |> List.exists (fun prop -> prop.Encoder <> "")
        [
            yield sprintf "    static member JsonEncoder : JsonEncoder<%s> =" param.Name
            yield sprintf "        fun (this : %s) ->" param.Name
            if isNotEmpty then
                yield sprintf "            E.object ["
                for prop in fields do
                    if prop.Encoder <> NoEncoder then
                        yield sprintf "                \"%s\", %s%sthis.%s" prop.Key prop.EncoderCall prop.CommentCode prop.Key.AsCodeMemberName
                yield sprintf "            ]"
            else
                yield sprintf "            E.object []"
        ]
    let getJsonDecoder (param : RecordParam) =
        let fields = meta.GetAllFields param.Name
        [
            yield sprintf "    static member JsonDecoder : JsonDecoder<%s> =" param.Name
            yield sprintf "        D.object (fun get ->"
            yield sprintf "            {"
            for prop in fields do
                let memberName = prop.Key.AsCodeMemberName
                if prop.Decoder = NoDecoder then
                    yield sprintf "                %s = (* %s *) %s" memberName prop.CommentCode prop.InitValue
                elif param.IsLoose && prop.InitValue <> NoInitValue then
                    if prop.Variation = FieldVariation.Option then
                        yield sprintf "                %s = get.Optional.Field %s\"%s\" %s" memberName prop.CommentCode prop.Key prop.Type.Decoder
                    else
                        yield sprintf "                %s = get.Optional.Field %s\"%s\" %s" memberName prop.CommentCode prop.Key prop.Decoder
                        yield sprintf "                    |> Option.defaultValue %s" prop.InitValue
                else
                    yield sprintf "                %s = get.Required.Field %s\"%s\" %s" memberName prop.CommentCode prop.Key prop.Decoder
            yield sprintf "            }"
            yield sprintf "        )"
        ]
    let getFieldSetter (param : RecordParam) (prop : FieldMeta) =
        [
            let memberName = prop.Key.AsCodeMemberName
            let varName = prop.Key.AsCodeVariableName
            yield sprintf "    static member Set%s (%s%s : %s) (this : %s) =" memberName prop.CommentCode varName prop.ValueType param.Name
            yield sprintf "        {this with %s = %s}" memberName varName
        ]
    let rec getComboHelper (param : RecordParam) ((name, combo) : string * ComboMeta) =
        if didInterfaceProcessed name then
            []
        else
            markInterfaceProcessed name
            (
                combo.Parents
                |> List.map ^<| getComboHelper param
                |> List.concat
            ) @ (
                combo.Fields
                |> List.map ^<| FieldMeta.SetFallbackComment name
                |> List.map ^<| getFieldSetter param
                |> List.concat
            )
    let rec getSelfComboHelper (param : RecordParam) =
        clearProcessedInterfaces ()
        getComboHelper param (param.Name, meta)
    let getRecordMiddle (param : RecordParam) =
        let fields = meta.GetAllFields param.Name
        [
            yield sprintf "} with"
            yield sprintf "    static member Create"
            yield sprintf "        ("
            let mutable index = 0
            for field in fields do
                index <- index + 1
                let comma = if index < fields.Length then "," else ""
                if field.InitValue = NoInitValue then
                    yield sprintf "            %s : %s%s%s" field.Key.AsCodeVariableName field.CommentCode field.ValueType comma
                elif field.Variation = FieldVariation.Option then
                    yield sprintf "            ?%s : %s%s%s" field.Key.AsCodeVariableName field.CommentCode field.Type.ValueType comma
                else
                    yield sprintf "            ?%s : %s%s%s" field.Key.AsCodeVariableName field.CommentCode field.ValueType comma
            yield sprintf "        ) : %s =" param.Name
            yield sprintf "        {"
            for field in fields do
                if field.InitValue = NoInitValue
                    || field.Variation = FieldVariation.Option then
                    yield sprintf "            %s = %s%s" field.Key.AsCodeMemberName field.CommentCode field.Key.AsCodeVariableName
                else
                    yield sprintf "            %s = %s%s" field.Key.AsCodeMemberName field.CommentCode field.Key.AsCodeVariableName
                    yield sprintf "                |> Option.defaultWith (fun () -> %s)" field.InitValue
            yield sprintf "        }"
        ] @ (
            getSelfComboHelper param
        ) @ (
            if param.IsJson then
                getJsonEncoder param
                @ getJsonDecoder param
                @ [
                    sprintf "    static member JsonSpec ="
                    sprintf "        FieldSpec.Create<%s> (%s.JsonEncoder, %s.JsonDecoder)" param.Name param.Name param.Name
                    sprintf "    interface IJson with"
                    sprintf "        member this.ToJson () = %s.JsonEncoder this" param.Name
                    sprintf "    interface IObj"
                ]
            else
                []
        )
    let getFieldAdder (prop : FieldMeta) =
        sprintf "    %s : %s%s" prop.Key.AsCodeMemberName prop.CommentCode prop.ValueType
    let rec getComboAdder (param : RecordParam) ((name, combo) : string * ComboMeta) =
        if didInterfaceProcessed name then
            []
        else
            markInterfaceProcessed name
            (
                combo.Parents
                |> List.map ^<| getComboAdder param
                |> List.concat
            ) @ (
                combo.Fields
                |> List.map ^<| FieldMeta.SetFallbackComment name
                |> List.map getFieldAdder
            )
    let rec getSelfComboAdder (param : RecordParam) =
        clearProcessedInterfaces ()
        getComboAdder param (param.Name, meta)
    let getFieldMember (param : RecordParam) (prop : FieldMeta) =
        [
            let memberName = prop.Key.AsCodeMemberName
            let varName = prop.Key.AsCodeVariableName
            yield sprintf "    member this.With%s (%s%s : %s) =" memberName prop.CommentCode varName prop.ValueType
            yield sprintf "        this |> %s.Set%s %s" param.Name memberName varName
        ]
    let rec getComboMember (param : RecordParam) ((name, combo) : string * ComboMeta) =
        if didInterfaceProcessed name then
            []
        else
            markInterfaceProcessed name
            (
                combo.Parents
                |> List.map ^<| getComboMember param
                |> List.concat
            ) @ (
                combo.Fields
                |> List.map ^<| FieldMeta.SetFallbackComment name
                |> List.map ^<| getFieldMember param
                |> List.concat
            )
    let rec getSelfComboMember (param : RecordParam) =
        clearProcessedInterfaces ()
        getComboMember param (param.Name, meta)

    interface IGenerator<RecordParam> with
        member __.Generate param =
            [
                getRecordHeader param
                getSelfComboAdder param
                getRecordMiddle param
                getSelfComboMember param
                meta.Parents
                |> List.filter (fun (parentName, parentMeta) -> parentName.StartsWith ("I"))
                |> InterfaceGenerator.GetImplementations InterfaceType.ValueInterface
            ]|> List.concat

type ClassGenerator (meta : ComboMeta) =
    let getClassHeader (param : ComboParam) =
        [
            if param.IsAbstract then
                yield "[<AbstractClass>]"
            yield sprintf "type %s (owner : IOwner, key : Key) =" param.Name
            yield sprintf "    inherit WrapProperties<%s, IComboProperty> ()" param.Name
            yield sprintf "    let target' = Properties.combo (owner, key)"
        ]
    let getClassMiddle' (param : ComboParam) =
        [
            yield sprintf "    do ("
            if param.IsFinal then
                yield sprintf "        target'.SealCombo ()"
            yield sprintf "        base.Setup (target')"
            yield sprintf "    )"
            yield sprintf "    static member Create (o, k) = new %s (o, k)" param.Name
            yield sprintf "    static member Create () = %s.Create (noOwner, NoKey)" param.Name
            yield sprintf "    static member AddToCombo key (combo : IComboProperty) ="
            yield sprintf "        combo.AddCustom<%s> (%s.Create, key)" param.Name param.Name
            yield sprintf "    override this.Self = this"
            yield sprintf "    override __.Spawn (o, k) = %s.Create (o, k)" param.Name
            yield sprintf "    override __.SyncTo t = target'.SyncTo t.Target"
        ]
    let getClassMiddle (param : ComboParam) =
        if param.IsAbstract then
            []
        else
            getClassMiddle' param
    let getFieldAdder (prop : FieldMeta) =
        let varName = prop.Key.AsCodeVariableName
        sprintf "    let %s = target'.%s" varName prop.AddPropCode

    let rec getComboAdder (param : ComboParam) ((name, combo) : string * ComboMeta) =
        if didInterfaceProcessed name then
            []
        else
            markInterfaceProcessed name
            (
                combo.Parents
                |> List.map ^<| getComboAdder param
                |> List.concat
            ) @ (
                combo.Fields
                |> List.map ^<| FieldMeta.SetFallbackComment name
                |> List.map getFieldAdder
            )
    let rec getSelfComboAdder (param : ComboParam) =
        clearProcessedInterfaces ()
        getComboAdder param (param.Name, meta)
    let getFieldMember (prop : FieldMeta) =
        sprintf "    member __.%s %s: %s = %s"
            prop.Key.AsCodeMemberName prop.CommentCode prop.PropType prop.Key.AsCodeVariableName
    let rec getComboMember (param : ComboParam) ((name, combo) : string * ComboMeta) =
        if didInterfaceProcessed name then
            []
        else
            markInterfaceProcessed name
            (
                combo.Parents
                |> List.map ^<| getComboMember param
                |> List.concat
            ) @ (
                combo.Fields
                |> List.map ^<| FieldMeta.SetFallbackComment name
                |> List.map getFieldMember
            )
    let rec getSelfComboMember (param : ComboParam) =
        clearProcessedInterfaces ()
        getComboMember param (param.Name, meta)
    interface IGenerator<ComboParam> with
        member __.Generate param =
            [
                getClassHeader param
                getSelfComboAdder param
                getClassMiddle param
                getSelfComboMember param
                meta.Parents
                |> List.filter (fun (parentName, parentMeta) -> parentName.StartsWith ("I"))
                |> InterfaceGenerator.GetImplementations InterfaceType.ComboInterface
            ]|> List.concat

type BuilderGenerator (meta : ComboMeta) =
    let getBuilderHeader (param : BuilderParam) =
        let requiredFields =
            meta.GetAllFields param.Name
            |> List.filter (fun f -> f.InitValue = NoInitValue)
        if requiredFields.Length = 0 then
            [
                yield sprintf "type %s () =" param.Name
                yield sprintf "    inherit ObjBuilder<%s> ()" param.Kind
                yield sprintf "    override __.Zero () = %s.Create ()" param.Kind
            ]
        else
            [
                yield sprintf "type %s" param.Name
                yield sprintf "        ("
                let mutable index = 0
                for field in requiredFields do
                    index <- index + 1
                    let comma = if index < requiredFields.Length then "," else ""
                    yield sprintf "            %s : %s%s%s" field.Key.AsCodeVariableName field.CommentCode field.ValueType comma
                yield sprintf "        ) ="
                yield sprintf "    inherit ObjBuilder<%s> ()" param.Kind
                yield sprintf "    override __.Zero () ="
                yield sprintf "        %s.Create (" param.Kind
                index <- 0
                for field in requiredFields do
                    index <- index + 1
                    let comma = if index < requiredFields.Length then "," else ""
                    yield sprintf "            %s = %s%s%s" field.Key.AsCodeVariableName field.CommentCode field.Key.AsCodeVariableName comma
                yield sprintf "        )"
            ]
    let getBuilderFooter (param : BuilderParam) =
        let requiredFieldNames =
            meta.GetAllFields param.Name
            |> List.filter (fun f -> f.InitValue = NoInitValue)
            |> List.map (fun f -> f.Key.AsCodeVariableName)
        if requiredFieldNames.Length = 0 then
            [
                sprintf ""
                sprintf "let %s = new %s ()" param.Key param.Name
            ]
        else
            let names = requiredFieldNames |> String.concat " "
            [
                sprintf ""
                sprintf "let %s %s =" param.Key <| String.concat " " requiredFieldNames
                sprintf "    new %s (%s)" param.Name <| String.concat ", " requiredFieldNames
            ]
    let getComboSetter (prop : FieldMeta) =
        let memberName = prop.Key.AsCodeMemberName
        let varName = prop.Key.AsCodeVariableName
        let propType = prop.Type.PropType
        let isVar = propType.StartsWith ("IVarProperty<")
        let isCombo = propType = "IComboProperty"
        [
            if isVar then
                match prop.Variation with
                | FieldVariation.NoVariation
                | FieldVariation.Option ->
                    yield sprintf "        target.%s.SetValue %s" memberName varName
                | FieldVariation.List ->
                    yield sprintf "        %s" varName
                    yield sprintf "        |> List.iter (fun v ->"
                    yield sprintf "            let prop = target.%s.Add ()" memberName
                    yield sprintf "            prop.SetValue v"
                    yield sprintf "        )"
                | FieldVariation.Dict ->
                    yield sprintf "        %s" varName
                    yield sprintf "        |> Map.iter (fun k v ->"
                    yield sprintf "            let prop = target.%s.Add (k)" memberName
                    yield sprintf "            prop.SetValue v"
                    yield sprintf "        )"
            elif isCombo then
                match prop.Variation with
                | FieldVariation.NoVariation ->
                    yield sprintf "        target.%s.SyncWith %s" memberName varName
                | _ ->
                    yield sprintf "        //NotSupported %s" prop.AsString
            else
                yield sprintf "        //NotSupported %s" prop.AsString
        ]
    let getOperation (param : BuilderParam) (prop : FieldMeta) =
        let propType = prop.Type.PropType
        let isVar = propType.StartsWith ("IVarProperty<")
        let isCombo = propType = "IComboProperty"
        let supported = isVar || isCombo
        [
            if supported then
                let memberName = prop.Key.AsCodeMemberName
                let varName = prop.Key.AsCodeVariableName
                yield sprintf "    [<CustomOperation(\"%s\")>]" prop.Key
                yield sprintf "    member __.%s (target : %s, %s%s : %s) =" memberName param.Kind prop.CommentCode varName prop.ValueType
                match param.Type with
                | ComboBuilder ->
                    for line in getComboSetter (prop) do
                        yield line
                    yield sprintf "        target"
                | ValueBuilder ->
                    yield sprintf "        target.With%s %s" memberName varName
        ]
    let rec getComboOperation (param : BuilderParam) ((name, combo) : string * ComboMeta) =
        if didInterfaceProcessed name then
            []
        else
            markInterfaceProcessed name
            (
                combo.Parents
                |> List.map ^<| getComboOperation param
                |> List.concat
            ) @ (
                combo.Fields
                |> List.map ^<| FieldMeta.SetFallbackComment name
                |> List.map ^<| getOperation param
                |> List.concat
            )
    let getSelfComboOperation (param : BuilderParam) =
        let name = param.Name.Replace ("Builder", "")
        clearProcessedInterfaces ()
        getComboOperation param (name, meta)
    interface IGenerator<BuilderParam> with
        member __.Generate param =
            [
                getBuilderHeader param
                getSelfComboOperation param
                getBuilderFooter param
            ]|> List.concat


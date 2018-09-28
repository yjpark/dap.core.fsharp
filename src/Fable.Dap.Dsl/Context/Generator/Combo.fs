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
    let getOperation (param : InterfaceParam) (prop : IPropMeta) =
        [
            match param.Type with
            | ComboInterface -> prop.PropType
            | ValueInterface -> prop.Type
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
                        | ValueInterface -> prop.Type
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
                    if prop.Encoder <> "" then
                        yield sprintf "                %s\"%s\", %sthis.%s" prop.CommentCode prop.Key prop.EncoderCall prop.Key.AsCodeMemberName
                yield sprintf "            ]"
            else
                yield sprintf "            E.object []"
        ]
    let getJsonDecoder (param : RecordParam) =
        let fields = meta.GetAllFields param.Name
        [
            yield sprintf "    static member JsonDecoder : JsonDecoder<%s> =" param.Name
            yield sprintf "        D.decode %s.Create" param.Name
            for prop in fields do
                if prop.Decoder = "" then
                    yield sprintf "        |> D.hardcoded %s(* %s *) %s" prop.CommentCode prop.Key prop.InitValue
                elif param.IsLoose && prop.InitValue <> "" then
                    yield sprintf "        |> D.optional %s\"%s\" %s %s" prop.CommentCode prop.Key prop.Decoder prop.InitValue
                else
                    yield sprintf "        |> D.required %s\"%s\" %s" prop.CommentCode prop.Key prop.Decoder
        ]
    let getFieldSetter (param : RecordParam) (prop : IPropMeta) =
        [
            let memberName = prop.Key.AsCodeMemberName
            let varName = prop.Key.AsCodeVariableName
            yield sprintf "    static member Set%s (%s%s : %s) (this : %s) =" memberName prop.CommentCode varName prop.Type param.Name
            yield sprintf "        {this with %s = %s}" memberName varName
        ]
    let getFieldUpdater (param : RecordParam) (prop : IPropMeta) =
        [
            let memberName = prop.Key.AsCodeMemberName
            yield sprintf "    static member Update%s (%supdate : %s -> %s) (this : %s) =" memberName prop.CommentCode prop.Type prop.Type param.Name
            yield sprintf "        this |> %s.Set%s (update this.%s)" param.Name memberName memberName
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
                |> List.map (fun prop -> prop.WithComment (Some name))
                |> List.map ^<| getFieldSetter param
                |> List.concat
            ) @ (
                combo.Fields
                |> List.map (fun prop -> prop.WithComment (Some name))
                |> List.map ^<| getFieldUpdater param
                |> List.concat
            )
    let rec getSelfComboHelper (param : RecordParam) =
        clearProcessedInterfaces ()
        getComboHelper param (param.Name, meta)
    let getRecordMiddle (param : RecordParam) =
        let fields = meta.GetAllFields param.Name
        let names =
            fields
            |> List.map (fun f -> f.Key.AsCodeVariableName)
            |> String.concat " "
        let noDefault =
            fields
            |> List.exists (fun f -> f.InitValue = "")
        [
            yield sprintf "} with"
            yield sprintf "    static member Create %s" names
            yield sprintf "            : %s =" param.Name
            yield sprintf "        {"
            for field in fields do
                yield sprintf "            %s = %s%s" field.Key.AsCodeMemberName field.CommentCode field.Key.AsCodeVariableName
            yield sprintf "        }"
            if not noDefault then
                yield sprintf "    static member Default () ="
                yield sprintf "        %s.Create" param.Name
                for f in fields do
                    yield sprintf "            %s(* %s *) %s" f.CommentCode f.Key.AsCodeVariableName f.InitValue
        ] @ (
            getSelfComboHelper param
        ) @ (
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
                |> List.map (fun prop -> prop.WithComment (Some name))
                |> List.map getFieldAdder
            )
    let rec getSelfComboAdder (param : RecordParam) =
        clearProcessedInterfaces ()
        getComboAdder param (param.Name, meta)
    let getFieldMember (param : RecordParam) (prop : IPropMeta) =
        [
            let memberName = prop.Key.AsCodeMemberName
            let varName = prop.Key.AsCodeVariableName
            yield sprintf "    member this.With%s (%s%s : %s) =" memberName prop.CommentCode varName prop.Type
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
                |> List.map (fun prop -> prop.WithComment (Some name))
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
                InterfaceGenerator.GetImplementations InterfaceType.ValueInterface meta.Parents
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
        let varName = prop.Key.AsCodeVariableName
        let validator =
            if prop.Validator = "" then
                "None"
            else
                sprintf "Some %s" prop.Validator
        match prop.Kind with
        | VarProperty ->
            match prop.Variation with
            | FieldVariation.List ->
                let prop = prop.ToVariant FieldVariation.Nothing
                sprintf "    let %s = target.AddList<%s%s> (%s, %s, \"%s\", %s, %s)"
                    varName prop.CommentCode prop.Type prop.Encoder prop.Decoder prop.Key prop.InitValue validator
            | FieldVariation.Dict ->
                let prop = prop.ToVariant FieldVariation.Nothing
                sprintf "    let %s = target.AddDict<%s%s> (%s, %s, \"%s\", %s, %s)"
                    varName prop.CommentCode prop.Type prop.Encoder prop.Decoder prop.Key prop.InitValue validator
            | FieldVariation.Option
            | FieldVariation.Nothing ->
                sprintf "    let %s = target.AddVar<%s%s> (%s, %s, \"%s\", %s, %s)"
                    varName prop.CommentCode prop.Type prop.Encoder prop.Decoder prop.Key prop.InitValue validator
        | ComboProperty ->
            match prop.Variation with
            | Nothing ->
                sprintf "    let %s = target.AddCombo %s(\"%s\")" varName prop.CommentCode prop.Key

            | List ->
                sprintf "    let %s = target.AddComboList %s(\"%s\")" varName prop.CommentCode prop.Key

            | Dict ->
                sprintf "    let %s = target.AddComboDict %s(\"%s\")" varName prop.CommentCode prop.Key

            | _ ->
                failWith "Unsupported" <| sprintf "%A<%s> %s %A" prop.Kind prop.Type prop.Key prop.Variation
        (*
        | CustomProperty ->
            match prop.Variation with
            | Nothing ->
                sprintf "    let %s = target.AddCustom<%s> %s(\"%s\")" varName prop.CommentCode prop.Key
            | _ ->
                failWith "Unsupported" <| sprintf "%A<%s> %s %A" prop.Kind prop.Type prop.Key prop.Variation
        *)
        | _ ->
            failWith "Unsupported" <| sprintf "%A<%s> %s %A" prop.Kind prop.Type prop.Key prop.Variation

    let rec getComboAdder (param : ClassParam) ((name, combo) : string * ComboMeta) =
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
                |> List.map (fun prop -> prop.WithComment (Some name))
                |> List.map getFieldAdder
            )
    let rec getSelfComboAdder (param : ClassParam) =
        clearProcessedInterfaces ()
        getComboAdder param (param.Name, meta)
    let getFieldMember (prop : IPropMeta) =
        sprintf "    member __.%s %s: %s = %s"
            prop.Key.AsCodeMemberName prop.CommentCode prop.PropType prop.Key
    let rec getComboMember (param : ClassParam) ((name, combo) : string * ComboMeta) =
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
                |> List.map (fun prop -> prop.WithComment (Some name))
                |> List.map getFieldMember
            )
    let rec getSelfComboMember (param : ClassParam) =
        clearProcessedInterfaces ()
        getComboMember param (param.Name, meta)
    interface IGenerator<ClassParam> with
        member __.Generate param =
            [
                getClassHeader param
                getSelfComboAdder param
                getClassMiddle param
                getSelfComboMember param
                InterfaceGenerator.GetImplementations InterfaceType.ComboInterface meta.Parents
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
    let getComboSetter (prop : IPropMeta) =
        let memberName = prop.Key.AsCodeMemberName
        let varName = prop.Key.AsCodeVariableName
        [
            match prop.Kind with
            | VarProperty ->
                match prop.Variation with
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
                | FieldVariation.Option
                | FieldVariation.Nothing ->
                    yield sprintf "        target.%s.SetValue %s" memberName varName
            | ComboProperty ->
                match prop.Variation with
                | FieldVariation.Nothing ->
                    yield sprintf "        target.%s.SyncWith %s" memberName varName
                | _ ->
                    yield sprintf "        //NotSupported %A %A" prop.Kind prop.Variation
            | _ ->
                yield sprintf "        //NotSupported %A %A" prop.Kind prop.Variation
        ]
    let getOperation (param : BuilderParam) (prop : IPropMeta) =
        let supported =
            match prop.Kind with
            | VarProperty ->
                prop.Decoder <> ""
            | ComboProperty ->
                true
            | _ ->
                false
        [
            if supported then
                let memberName = prop.Key.AsCodeMemberName
                let varName = prop.Key.AsCodeVariableName
                yield sprintf "    [<CustomOperation(\"%s\")>]" prop.Key
                yield sprintf "    member __.%s (target : %s, %s%s : %s) =" memberName param.Kind prop.CommentCode varName prop.Type
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
                |> List.map (fun prop -> prop.WithComment (Some name))
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


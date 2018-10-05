[<AutoOpen>]
[<RequireQualifiedAccess>]
module Dap.Prelude.Union

open Microsoft.FSharp.Reflection

let getKind<'a> (x : 'a) =
    match FSharpValue.GetUnionFields(x, typeof<'a>) with
    | kind, _ -> kind.Name

let tryFindCase<'a> (kind : string) =
    match FSharpType.GetUnionCases typeof<'a> |> Array.filter (fun case -> case.Name = kind) with
    |[|case|] ->
        Ok case
    |_ ->
        Error <| sprintf "Invalid_Kind: <%s> %s" typeof<'a>.FullName kind

let tryFromKind<'a> (kind : string) =
    tryFindCase<'a> (kind)
    |> Result.map (fun case ->
        FSharpValue.MakeUnion(case, [||]) :?> 'a
    )

let findCase<'a> (kind : string) =
    tryFindCase<'a> (kind)
    |> Result.get

let fromKind<'a> (kind : string) =
    tryFromKind<'a> (kind)
    |> Result.get

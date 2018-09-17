[<AutoOpen>]
[<RequireQualifiedAccess>]
module Dap.Prelude.Union

open Microsoft.FSharp.Reflection
#if FABLE_COMPILER
open Fable.Core
#endif

#if FABLE_COMPILER
[<PassGenericsAttribute>]
#endif
let getKind<'a> (x : 'a) =
    match FSharpValue.GetUnionFields(x, typeof<'a>) with
    | kind, _ -> kind.Name

#if FABLE_COMPILER
[<PassGenericsAttribute>]
#endif
let tryFindCase<'a> (kind : string) =
    match FSharpType.GetUnionCases typeof<'a> |> Array.filter (fun case -> case.Name = kind) with
    |[|case|] ->
        Ok case
    |_ ->
        Error <| sprintf "Invalid_Kind: <%s> %s" typeof<'a>.FullName kind

#if FABLE_COMPILER
[<PassGenericsAttribute>]
#endif
let tryFromKind<'a> (kind : string) =
    tryFindCase<'a> (kind)
    |> Result.map (fun case ->
        FSharpValue.MakeUnion(case, [||]) :?> 'a
    )

#if FABLE_COMPILER
[<PassGenericsAttribute>]
#endif
let findCase<'a> (kind : string) =
    tryFindCase<'a> (kind)
    |> Result.get

#if FABLE_COMPILER
[<PassGenericsAttribute>]
#endif
let fromKind<'a> (kind : string) =
    tryFromKind<'a> (kind)
    |> Result.get

[<AutoOpen>]
[<RequireQualifiedAccess>]
module Dap.Prelude.Union

open Microsoft.FSharp.Reflection
#if FABLE_COMPILER
open Fable.Core
#endif

#if FABLE_COMPILER
let [<PassGenericsAttribute>] getKind<'a> (x : obj) = 
#else
let getKind<'a> (x : obj) = 
#endif
    match FSharpValue.GetUnionFields(x, typeof<'a>) with
    | kind, _ -> kind.Name

#if FABLE_COMPILER
let [<PassGenericsAttribute>] fromKind<'a> (s : string) =
#else
let fromKind<'a> (s : string) =
#endif
    match FSharpType.GetUnionCases typeof<'a> |> Array.filter (fun case -> case.Name = s) with
    |[|case|] ->
        Ok (FSharpValue.MakeUnion(case, [||]) :?> 'a)
    |_ ->
        Error <| sprintf "Decode_Failed: <%s> %s" typeof<'a>.FullName s

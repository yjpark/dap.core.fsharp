module Dap.Context.Meta.Util

open Microsoft.FSharp.Quotations
module QP = Microsoft.FSharp.Quotations.Patterns

open Dap.Prelude
open Dap.Context

//http://www.fssnip.net/h1/title/Eval-Quotations
let unquotePropertyGetExpr<'obj> (expr : Expr<'obj>) : string * 'obj =
    match expr with
    | QP.PropertyGet (None, p, []) ->
        (p.Name, p.GetValue(null, [| |]) :?> 'obj)
    | _ ->
        failWith "Unsupported_Expression" <| expr.ToString ()

//https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/keyword-reference
let FSharpKeywords =
    [
        "abstract and as assert base begin class default delegate do done"
        "downcast downto elif else end exception extern false finally fixed"
        "for fun function global if in inherit inline interface internal lazy"
        "let match member module mutable namespace new not null of open or"
        "override private public rec return select static struct then to true"
        "try type upcast use val void when while with yield"
        "asr land lor lsl lsr lxor mod sig"
        "atomic break checked component const constraint constructor continue"
        "eager event external functor include method mixin object parallel"
        "process protected pure sealed tailcall trait virtual volatile"

    ]|> List.map (fun words -> words.Split (' ') |> Array.toList)
    |> List.concat

let escapeKeyword (key : string) =
    FSharpKeywords
    |> List.exists (fun k -> k = key)
    |> function
        | true -> key + "'"
        | false -> key

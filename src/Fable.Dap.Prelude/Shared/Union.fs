[<AutoOpen>]
module Dap.Prelude.Union

open Microsoft.FSharp.Reflection
#if FABLE_COMPILER
open Fable.Core
#endif

(*
 * It's a bit ugly now, these function used to be normal functions, though
 * Fable2 remove PassGeneric, but the inline way is not working well here
 * maybe due to these functions are used deeply by other modules such as EDS
 * and UnionSpec, and ITypeResolver<> only working with methods injection (AFAIK)
 *)
type Union = UnionHelper with
    static member getKind<'u> (x : 'u
        #if FABLE_COMPILER
            , [<Inject>] ?resolver: ITypeResolver<'u>
        #endif
        ) : string =
    #if FABLE_COMPILER
        let uType = resolver.Value.ResolveType ()
    #else
        let uType = typeof<'u>
    #endif
        match FSharpValue.GetUnionFields(x, uType) with
        | kind, _ -> kind.Name
    static member tryFindCase<'u> (kind : string
        #if FABLE_COMPILER
            , [<Inject>] ?resolver: ITypeResolver<'u>
        #endif
            ) : Result<UnionCaseInfo, string> =
    #if FABLE_COMPILER
        let uType = resolver.Value.ResolveType ()
    #else
        let uType = typeof<'u>
    #endif
        match FSharpType.GetUnionCases uType |> Array.filter (fun case -> case.Name = kind) with
        |[|case|] ->
            Ok case
        |_ ->
            Error <| sprintf "Invalid_Kind: <%s> %s" uType.FullName kind
    static member tryFromKind<'u> (kind : string
        #if FABLE_COMPILER
            , [<Inject>] ?resolver: ITypeResolver<'u>
        #endif
            ) : Result<'u, string> =
    #if FABLE_COMPILER
        Union.tryFindCase<'u> (kind, ?resolver=resolver)
    #else
        Union.tryFindCase<'u> kind
    #endif
        |> Result.map (fun case ->
            FSharpValue.MakeUnion(case, [||]) :?> 'u
        )
    static member findCase<'u> (kind : string
        #if FABLE_COMPILER
            , [<Inject>] ?resolver: ITypeResolver<'u>
        #endif
            ) : UnionCaseInfo =
    #if FABLE_COMPILER
        Union.tryFindCase<'u> (kind, ?resolver=resolver)
    #else
        Union.tryFindCase<'u> kind
    #endif
        |> Result.get
    static member fromKind<'u> (kind : string
        #if FABLE_COMPILER
            , [<Inject>] ?resolver: ITypeResolver<'u>
        #endif
            ) : 'u =
    #if FABLE_COMPILER
        Union.tryFromKind<'u> (kind, ?resolver=resolver)
    #else
        Union.tryFromKind<'u> kind
    #endif
        |> Result.get

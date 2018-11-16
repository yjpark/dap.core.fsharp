[<AutoOpen>]
module Dap.Prelude.Operator

open System
#if FABLE_COMPILER
open Fable.Core
#endif

/// To remove some parenthesis
/// http://kevincantu.org/code/operators.html
let inline (^<|) f a = f a

let (=?) a b = LanguagePrimitives.PhysicalEquality a b

let (<>?) a b = not <| LanguagePrimitives.PhysicalEquality a b

let typeNameOf<'t> () =
    #if FABLE_COMPILER
        "_?_"
    #else
        typeof<'t>.FullName
    #endif

let getTypeName (v : obj) =
    #if FABLE_COMPILER
        "_?_"
    #else
        (v.GetType ()) .FullName
    #endif

type Fable = FableHelper with
    static member TypeOf<'t> (v : 't
            #if FABLE_COMPILER
                , [<Inject>] ?resolver: ITypeResolver<'t>
            #endif
        ) : Type =
    #if FABLE_COMPILER
        resolver.Value.ResolveType ()
    #else
        typeof<'t>
    #endif

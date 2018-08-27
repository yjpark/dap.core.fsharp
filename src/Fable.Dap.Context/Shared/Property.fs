[<AutoOpen>]
[<RequireQualifiedAccess>]
module Dap.Context.Property

open Dap.Prelude
open Dap.Context.Internal

let spec key initValue =
    IPropertySpec.Create (key, initValue)

let boolSpec key initValue validator =
    IVarPropertySpec<bool>.Create (key, E.bool, D.bool, initValue, validator)

let intSpec key initValue validator =
    IVarPropertySpec<int>.Create (key, E.int, D.int, initValue, validator)

#if !FABLE_COMPILER
let longSpec key initValue validator =
    IVarPropertySpec<int64>.Create (key, E.long, D.long, initValue, validator)
#endif

let stringSpec key initValue validator =
    VarPropertySpec<string>.Create (key, E.string, D.string, initValue, validator)

[<AutoOpen>]
[<RequireQualifiedAccess>]
module Dap.Context.Property

open Dap.Prelude
open Dap.Context.Internal

let spec key initValue =
    PropertySpec.Create key key initValue
    :> IPropertySpec

let boolSpec key initValue validator =
    PropertySpec<bool>.Create E.bool D.bool key key initValue validator
    :> IPropertySpec<bool>

let intSpec key initValue validator =
    PropertySpec<int>.Create E.int D.int key key initValue validator
    :> IPropertySpec<int>

#if !FABLE_COMPILER
let longSpec key initValue validator =
    PropertySpec<int64>.Create E.long D.long key key initValue validator
    :> IPropertySpec<int64>
#endif

let stringSpec key initValue validator =
    PropertySpec<string>.Create E.string D.string key key initValue validator
    :> IPropertySpec<string>

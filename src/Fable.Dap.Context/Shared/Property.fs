[<AutoOpen>]
[<RequireQualifiedAccess>]
module Dap.Context.Property

open Dap.Prelude
open Dap.Context.Internal

let mapSpec<'p when 'p :> IProperty> key initValue spawner =
    PropertySpec<'p>.Create2 key initValue spawner

let listSpec<'p when 'p :> IProperty> key initValue spawner =
    PropertySpec<'p>.Create2 key initValue spawner

let comboSpec key initValue =
    PropertySpec.Create1 key initValue

let customSpec<'p when 'p :> IProperty> key initValue spawner =
    PropertySpec<'p>.Create2 key initValue spawner

let varSpec<'v> encoder decoder key initValue validator =
    VarPropertySpec<'v>.Create key encoder decoder initValue validator

let boolSpec key initValue validator =
    VarPropertySpec<bool>.Create key E.bool D.bool initValue validator

let stringSpec key initValue validator =
    VarPropertySpec<string>.Create key E.string D.string initValue validator

let intSpec key initValue validator =
    VarPropertySpec<int>.Create key E.int D.int initValue validator

#if !FABLE_COMPILER
let longSpec key initValue validator =
    VarPropertySpec<int64>.Create key E.long D.long initValue validator
#endif

let decimalSpec key initValue validator =
    VarPropertySpec<decimal>.Create key E.decimal D.decimal initValue validator

let bool owner key initValue validator =
    boolSpec key initValue validator
    |> fun spec -> spec.Spawner owner key

let string owner key initValue validator =
    stringSpec key initValue validator
    |> fun spec -> spec.Spawner owner key

let int owner key initValue validator =
    intSpec key initValue validator
    |> fun spec -> spec.Spawner owner key

#if !FABLE_COMPILER
let long owner key initValue validator =
    longSpec key initValue validator
    |> fun spec -> spec.Spawner owner key
#endif

let decimal owner key initValue validator =
    decimalSpec key initValue validator
    |> fun spec -> spec.Spawner owner key

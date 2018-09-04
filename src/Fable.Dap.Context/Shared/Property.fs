[<AutoOpen>]
[<RequireQualifiedAccess>]
module Dap.Context.Property

open Dap.Prelude
open Dap.Context.Internal

let mapSpec<'p when 'p :> IProperty> key initValue spawner =
    PropertySpec<'p>.Create2 PK_Map key initValue spawner

let listSpec<'p when 'p :> IProperty> key initValue spawner =
    PropertySpec<'p>.Create2 PK_List key initValue spawner

let comboSpec key initValue =
    PropertySpec.Create1 PK_Combo key initValue

let customSpec<'p when 'p :> IProperty> kind key initValue spawner =
    PropertySpec<'p>.Create2 kind key initValue spawner

let varSpec<'v> kind encoder decoder key initValue validator =
    VarPropertySpec<'v>.Create kind key encoder decoder initValue validator

let boolSpec key initValue validator =
    VarPropertySpec<bool>.Create PK_Bool key E.bool D.bool initValue validator

let intSpec key initValue validator =
    VarPropertySpec<int>.Create PK_Int key E.int D.int initValue validator

#if !FABLE_COMPILER
let longSpec key initValue validator =
    VarPropertySpec<int64>.Create PK_Long key E.long D.long initValue validator
#endif

let stringSpec key initValue validator =
    VarPropertySpec<string>.Create PK_String key E.string D.string initValue validator

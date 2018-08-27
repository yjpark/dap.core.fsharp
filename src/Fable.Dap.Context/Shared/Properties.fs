[<AutoOpen>]
[<RequireQualifiedAccess>]
module Dap.Context.Properties

open Dap.Prelude
open Dap.Context.Internal

type IProperties with
    member this.AddBool key initValue validator =
        Property.boolSpec key initValue None
        |> this.Add<bool>
    member this.AddInt key initValue validator =
        Property.intSpec key initValue None
        |> this.Add<int>
#if !FABLE_COMPILER
    member this.AddLong key initValue validator =
        Property.longSpec key initValue None
        |> this.Add<int64>
#endif
    member this.AddString key initValue validator =
        Property.stringSpec key initValue None
        |> this.Add<string>

let create (owner : IOwner) key initValue =
    PropertySpec.Create key key initValue
    |> Properties.Create owner
    :> IProperties
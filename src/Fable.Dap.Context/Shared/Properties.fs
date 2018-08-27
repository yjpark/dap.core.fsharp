[<AutoOpen>]
[<RequireQualifiedAccess>]
module Dap.Context.Properties

open Dap.Prelude
open Dap.Context.Internal

type IComboProperty with
    member this.AddBool key initValue validator =
        Property.boolSpec key initValue None
        |> this.AddVar<bool>
    member this.AddInt key initValue validator =
        Property.intSpec key initValue None
        |> this.AddVar<int>
#if !FABLE_COMPILER
    member this.AddLong key initValue validator =
        Property.longSpec key initValue None
        |> this.AddVar<int64>
#endif
    member this.AddString key initValue validator =
        Property.stringSpec key initValue None
        |> this.AddVar<string>

let combo (owner : IOwner) key =
    PropertySpec.Create key key <| E.object []
    |> ComboProperty.Create owner
    :> IComboProperty
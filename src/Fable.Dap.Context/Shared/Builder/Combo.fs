[<RequireQualifiedAccess>]
module Dap.Context.Builder.Combo

open Microsoft.FSharp.Quotations

open Dap.Prelude
open Dap.Context

type Builder () =
    inherit ObjBuilder<IComboProperty> ()
    override __.Zero () =
        IComboProperty.Default ()
    [<CustomOperation("var")>]
    member __.Var (combo : IComboProperty, prop : IVarProperty) =
        combo.AddAny prop.Spec0.Key prop.Clone0 |> ignore
        combo
    [<CustomOperation("custom")>]
    member __.Custom (combo : IComboProperty, prop : ICustomProperty) =
        combo.AddAny prop.Spec0.Key prop.Clone0 |> ignore
        combo
    [<CustomOperation("combo")>]
    member __.Combo (combo : IComboProperty, prop : IComboProperty) =
        combo.AddAny prop.Spec0.Key prop.Clone0 |> ignore
        combo

type ExtendBuilder (parent : IComboProperty) =
    inherit Builder ()
    override __.Zero () = parent.Clone (noOwner, NoKey)
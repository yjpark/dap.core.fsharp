module Dap.Platform.Demo.Builder

open Dap.Context
open Dap.Context.Builder

open Dap.Platform.Demo.Types

(*
 * Generated: Builder<PublisherBuilder>
    {
        "name": "John Doe",
        "year": 2000
    }
 *)
type PublisherBuilder () =
    inherit ObjBuilder<PublisherProperty> ()
    override __.Zero () = PublisherProperty.Empty ()
    [<CustomOperation("name")>]
    member __.Name (target : PublisherProperty, v) =
        target.Name.SetValue v |> ignore
        target
    [<CustomOperation("year")>]
    member __.Year (target : PublisherProperty, v) =
        target.Year.SetValue v |> ignore
        target

let publisher = PublisherBuilder ()
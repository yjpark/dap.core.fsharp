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
    member __.Name (this : PublisherProperty, v) =
        this.Name.SetValue v |> ignore
        this
    [<CustomOperation("year")>]
    member __.Year (this : PublisherProperty, v) =
        this.Year.SetValue v |> ignore
        this

let publisher = PublisherBuilder ()
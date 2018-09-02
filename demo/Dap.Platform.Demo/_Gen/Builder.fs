module Dap.Platform.Demo.Builder

open Dap.Context
open Dap.Context.Builder

open Dap.Platform.Demo.Types

(*
 * Generated: Builder<AuthorBuilder>
    {
        "age": 30,
        "name": "John Doe",
        "publisher": "No Publisher"
    }
 *)
type AuthorBuilder () =
    inherit ObjBuilder<Author> ()
    override __.Zero () = Author.Empty ()
    [<CustomOperation("age")>]
    member __.Age (target : Author, v) =
        target.Age.SetValue v |> ignore
        target
    [<CustomOperation("name")>]
    member __.Name (target : Author, v) =
        target.Name.SetValue v |> ignore
        target
    [<CustomOperation("publisher")>]
    member __.Publisher (target : Author, v) =
        target.Publisher.SetValue v |> ignore
        target

let author = AuthorBuilder ()
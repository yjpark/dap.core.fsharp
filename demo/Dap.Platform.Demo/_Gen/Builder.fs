module Dap.Platform.Demo.Builder

open Dap.Context
open Dap.Context.Builder

open Dap.Platform.Demo.Types

(*
 * Generated: <Builder>
 *)
type AuthorBuilder () =
    inherit ObjBuilder<Author> ()
    override __.Zero () = Author.Empty ()
    [<CustomOperation("name")>]
    member __.Name (target : Author, v) =
        target.Name.SetValue v
        target
    [<CustomOperation("age")>]
    member __.Age (target : Author, v) =
        target.Age.SetValue v
        target
    [<CustomOperation("publisher")>]
    member __.Publisher (target : Author, v) =
        target.Publisher.SetValue v
        target

let author = AuthorBuilder ()
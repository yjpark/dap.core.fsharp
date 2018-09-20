module Demo.Builder

open Dap.Prelude
open Dap.Context
open Dap.Context.Builder

(*
 * Generated: <ComboBuilder>
 *)
type AuthorBuilder () =
    inherit ObjBuilder<Author> ()
    override __.Zero () = Author.Default ()
    [<CustomOperation("name")>]
    member __.Name (target : Author, name : string) =
        target.Name.SetValue name
        target
    [<CustomOperation("age")>]
    member __.Age (target : Author, age : int) =
        target.Age.SetValue age
        target
    [<CustomOperation("publisher")>]
    member __.Publisher (target : Author, publisher : string) =
        target.Publisher.SetValue publisher
        target

let author = AuthorBuilder ()
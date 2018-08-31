[<AutoOpen>]
module Dap.Context.Builder.Types

open Dap.Context

[<AbstractClass>]
type ObjBuilder<'obj when 'obj :> IObj> () =
    member this.Yield (_ : 'a) = this.Zero ()
    abstract Zero : unit -> 'obj

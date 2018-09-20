[<AutoOpen>]
module Dap.Context.Builder.Types

#if FABLE_COMPILER
open Fable.Core
#endif

open Dap.Context

[<AbstractClass>]
type ObjBuilder<'obj when 'obj :> IObj> () =
#if FABLE_COMPILER
    [<PassGenericsAttribute>]
#endif
    member this.Yield (_ : 'a) = this.Zero ()
#if FABLE_COMPILER
    [<PassGenericsAttribute>]
#endif
    abstract Zero : unit -> 'obj
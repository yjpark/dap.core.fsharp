[<AutoOpen>]
[<RequireQualifiedAccess>]
module Dap.Context.Context

open Dap.Prelude
open Dap.Context.Internal

let create (logging : ILogging) kind initValue =
    ContextSpec.Create kind initValue
    |> Context.Create logging
    :> IContext
[<AutoOpen>]
module Dap.Platform.Runner

open Dap.Prelude

type IRunner =
    inherit ILogger

and IRunner<'runner when 'runner :> IRunner> =
    abstract Runner : 'runner with get
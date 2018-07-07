[<AutoOpen>]
module Dap.Platform.Runner

open Dap.Prelude

type IRunner =
    inherit ILogger
    abstract Clock : IClock with get

and IRunner<'runner when 'runner :> IRunner> =
    abstract Runner : 'runner with get
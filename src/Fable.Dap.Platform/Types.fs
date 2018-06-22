[<AutoOpen>]
module Dap.Platform.Types'

open Dap.Prelude

type IRunner =
    inherit ILogger

and IRunner<'runner> =
    inherit IRunner
    abstract Self : 'runner with get

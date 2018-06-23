[<AutoOpen>]
module Dap.Platform.Types'

open Dap.Prelude

type IRunner =
    inherit ILogger

type IRunner<'runner> =
    inherit IRunner
    abstract Self' : 'runner with get

type IAgent =
    inherit IOwner
    inherit IRunner<IAgent>
    abstract Ident : Ident with get

and IAgent<'req, 'evt> =
    inherit IAgent
    inherit IPoster<'req>
    abstract Actor : IActor<'req, 'evt> with get

and IAgent<'model, 'req, 'evt> =
    inherit IAgent<'req, 'evt>
    abstract Actor : IActor<'model, 'req, 'evt> with get
